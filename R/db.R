# R/db.R — připojení k PostgreSQL (pool) + konfigurace + SQL helpery

if (!requireNamespace("pool", quietly = TRUE))      stop("Install 'pool'")
if (!requireNamespace("DBI", quietly = TRUE))       stop("Install 'DBI'")
if (!requireNamespace("RPostgres", quietly = TRUE)) stop("Install 'RPostgres'")
if (!requireNamespace("yaml", quietly = TRUE))      stop("Install 'yaml'")

APP_CONFIG_FILE <- Sys.getenv("APP_CONFIG_FILE", "app_config.yml")

load_app_config <- function(){
  if (file.exists(APP_CONFIG_FILE)) yaml::read_yaml(APP_CONFIG_FILE) else list(
    host = Sys.getenv("APP_DB_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("APP_DB_PORT", "5432")),
    dbname = Sys.getenv("APP_DB_NAME", "budgeting"),
    user = Sys.getenv("APP_DB_USER", "budget"),
    password = Sys.getenv("APP_DB_PASSWORD", "budgetpass")
  )
}
save_app_config <- function(cfg){ yaml::write_yaml(cfg, APP_CONFIG_FILE); APP_CONFIG_FILE }

# Pool (lazy singleton)
.APP_DB_POOL <- NULL
get_db_pool <- function(force_reload = FALSE){
  if (force_reload && !is.null(.APP_DB_POOL)) try(pool::poolClose(.APP_DB_POOL), silent = TRUE)
  if (is.null(.APP_DB_POOL) || force_reload){
    cfg <- load_app_config()
    .APP_DB_POOL <<- pool::dbPool(
      drv = RPostgres::Postgres(),
      host = cfg$host, port = as.integer(cfg$port), dbname = cfg$dbname,
      user = cfg$user, password = cfg$password,
      bigint = "numeric", idleTimeout = 60
    )
  }
  .APP_DB_POOL
}
close_db_pool <- function(){ if (!is.null(.APP_DB_POOL)) pool::poolClose(.APP_DB_POOL) }

# Utility
metadata_exists <- function(pool){
  ok <- DBI::dbGetQuery(pool,
                        "select exists(select 1 from information_schema.schemata where schema_name='app_meta') as ok")$ok
  isTRUE(ok)
}
db_test_connection <- function(cfg){
  con <- try(DBI::dbConnect(RPostgres::Postgres(),
                            host=cfg$host, port=as.integer(cfg$port),
                            dbname=cfg$dbname, user=cfg$user, password=cfg$password), silent=TRUE)
  if (inherits(con, "try-error")) return(list(ok=FALSE, msg=as.character(con)))
  on.exit(DBI::dbDisconnect(con), add=TRUE)
  ok <- try(DBI::dbGetQuery(con, "select 1 as ok"), silent=TRUE)
  if (inherits(ok, "try-error")) list(ok=FALSE, msg=as.character(ok)) else list(ok=TRUE, msg="OK")
}

# SQL helpery (auth/users)
sql_get_user_by_email <- function(pool, email){
  DBI::dbGetQuery(pool,
                  "select user_id, email::text as email, display_name, password_hash, is_active
       from app_meta.user_account where email=$1",
                  params = list(email)
  )
}
sql_get_roles_for_user <- function(pool, user_id){
  DBI::dbGetQuery(pool,
                  "select r.code from app_meta.user_role ur
      join app_meta.role r on r.role_id=ur.role_id
     where ur.user_id=$1",
                  params = list(user_id)
  )$code
}
sql_touch_last_login <- function(pool, user_id){
  DBI::dbExecute(pool, "update app_meta.user_account set last_login_at=now() where user_id=$1",
                 params=list(user_id))
}
sql_session_login <- function(pool, session_id, user_id, ip = NULL, ua = NULL){
  DBI::dbExecute(
    pool,
    "
    insert into app_meta.session_log(session_id, user_id, ip, user_agent, login_at, logout_at)
    values ($1, $2, $3, $4, now(), null)
    on conflict (session_id) do update
      set user_id   = excluded.user_id,
          ip        = excluded.ip,
          user_agent= excluded.user_agent,
          login_at  = excluded.login_at,
          logout_at = null
    ",
    params = list(session_id, user_id, ip, ua)
  )
}

sql_session_logout <- function(pool, session_id){
  DBI::dbExecute(pool, "update app_meta.session_log set logout_at=now() where session_id=$1",
                 params=list(session_id))
}
sql_list_users <- function(pool){
  DBI::dbGetQuery(pool, "select * from app_meta.v_user_roles order by user_id")
}
sql_insert_user <- function(pool, email, display_name, password_hash,
                            roles = c("user"), is_active = TRUE){
  pool::poolWithTransaction(pool, function(con){
    uid <- DBI::dbGetQuery(
      con,
      "insert into app_meta.user_account(email, display_name, password_hash, is_active)
       values($1,$2,$3,$4) returning user_id",
      params = list(email, display_name, password_hash, is_active)
    )$user_id
    
    if (length(roles)) {
      ph <- paste(sprintf("$%d", seq_along(roles)), collapse = ",")
      role_ids <- DBI::dbGetQuery(
        con,
        paste0("select role_id from app_meta.role where code in (", ph, ")"),
        params = as.list(roles)
      )$role_id
      for (rid in role_ids){
        DBI::dbExecute(
          con,
          "insert into app_meta.user_role(user_id, role_id)
           values($1,$2) on conflict do nothing",
          params = list(uid, rid)
        )
      }
    }
    uid
  })
}

sql_update_user_active <- function(pool, user_id, is_active){
  DBI::dbExecute(pool, "update app_meta.user_account set is_active=$2 where user_id=$1",
                 params=list(user_id, is_active))
}
sql_set_user_roles <- function(pool, user_id, roles){
  pool::poolWithTransaction(pool, function(con){
    DBI::dbExecute(con,
                   "delete from app_meta.user_role where user_id=$1",
                   params = list(user_id)
    )
    if (length(roles)) {
      ph <- paste(sprintf("$%d", seq_along(roles)), collapse = ",")
      role_ids <- DBI::dbGetQuery(
        con,
        paste0("select role_id from app_meta.role where code in (", ph, ")"),
        params = as.list(roles)
      )$role_id
      for (rid in role_ids){
        DBI::dbExecute(con,
                       "insert into app_meta.user_role(user_id, role_id) values($1,$2)",
                       params = list(user_id, rid)
        )
      }
    }
  })
}


sql_reset_password <- function(pool, user_id, password_hash){
  DBI::dbExecute(pool, "update app_meta.user_account set password_hash=$2 where user_id=$1",
                 params=list(user_id, password_hash))
}

# SQL helpery (notifikace)
sql_insert_notification <- function(pool, message, user_id = NULL){
  if (is.null(user_id)) {
    uids <- DBI::dbGetQuery(pool,
                            "select user_id from app_meta.user_account where is_active")$user_id
    for (uid in uids) {
      DBI::dbExecute(pool,
                     "insert into app_meta.notification(user_id, message) values($1,$2)",
                     params = list(uid, message))
    }
  } else {
    DBI::dbExecute(pool,
                   "insert into app_meta.notification(user_id, message) values($1,$2)",
                   params = list(user_id, message))
  }
}

sql_get_notifications <- function(pool, user_id){
  DBI::dbGetQuery(
    pool,
    "select notification_id, message, created_at\n       from app_meta.notification\n      where user_id=$1 and not is_read\n      order by created_at desc",
    params = list(user_id)
  )
}

sql_mark_notifications_read <- function(pool, user_id){
  DBI::dbExecute(pool,
                 "update app_meta.notification set is_read=true where user_id=$1 and not is_read",
                 params = list(user_id))
}
