# R/mod_setup.R — instalační wizard (DB připojení, inicializace, admin)

if (!requireNamespace("yaml", quietly = TRUE))      stop("Install 'yaml'")
if (!requireNamespace("RPostgres", quietly = TRUE)) stop("Install 'RPostgres'")

# robustní split SQL (respektuje $$ ... $$ bloky)
split_sql <- function(ddl){
  ch <- strsplit(ddl, "", fixed=TRUE)[[1]]
  in_dollar <- FALSE
  buf <- ""; stmts <- character()
  i <- 1
  while (i <= length(ch)){
    if (ch[i] == "$" && i < length(ch) && ch[i+1] == "$"){
      in_dollar <- !in_dollar
      buf <- paste0(buf, "$$")
      i <- i + 2
      next
    }
    if (!in_dollar && ch[i] == ";"){
      if (nzchar(trimws(buf))) stmts <- c(stmts, buf)
      buf <- ""; i <- i + 1; next
    }
    buf <- paste0(buf, ch[i]); i <- i + 1
  }
  if (nzchar(trimws(buf))) stmts <- c(stmts, buf)
  trimws(stmts)
}
run_ddl <- function(con, ddl){
  stmts <- if (length(ddl) == 1) split_sql(ddl) else ddl
  for (s in stmts){ if (nzchar(s)) DBI::dbExecute(con, s) }
}

# --- UI: Instalační průvodce (shinydashboardPlus) ---
# --- UI: Instalační průvodce (bs4Dash) ---
mod_setup_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        bs4Box(
          title = tagList(icon("database"), span(" Krok 1 – Připojení k databázi")),
          status = "info", solidHeader = TRUE, width = 12, closable = FALSE,
          textInput(ns("host"),   "Host",   value = "127.0.0.1"),
          numericInput(ns("port"), "Port",  value = 5432, min = 1, max = 65535),
          textInput(ns("dbname"), "Databáze", value = "budgeting"),
          textInput(ns("user"),   "Uživatel", value = "budget"),
          passwordInput(ns("password"), "Heslo"),
          actionButton(ns("test"),    "Otestovat připojení", icon = icon("plug"), class = "btn btn-primary"),
          actionButton(ns("savecfg"), "Uložit konfiguraci",  icon = icon("save")),
          actionButton(ns("loadcfg"), "Načíst z konfigurace",icon = icon("upload")),
          uiOutput(ns("conn_status"))
        )
      ),
      column(
        width = 6,
        bs4Box(
          title = tagList(icon("cogs"), span(" Krok 2 – Inicializace a admin")),
          status = "warning", solidHeader = TRUE, width = 12, closable = FALSE,
          uiOutput(ns("meta_status")),
          actionButton(ns("init"),
                       "Inicializovat metadata (vytvořit schéma app_meta)",
                       icon = icon("cog"), class = "btn btn-warning"
          ),
          tags$hr(),
          textInput(ns("admin_email"), "Admin e-mail"),
          textInput(ns("admin_name"),  "Admin jméno"),
          passwordInput(ns("admin_pw"), "Admin heslo"),
          actionButton(ns("mkadmin"), "Vytvořit admina", icon = icon("user-shield"),
                       class = "btn btn-success"),
          uiOutput(ns("admin_status"))
        )
      )
    )
  )
}



mod_setup_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$loadcfg, {
      cfg <- load_app_config()
      updateTextInput(session, "host",   value = cfg$host)
      updateNumericInput(session, "port", value = as.integer(cfg$port))
      updateTextInput(session, "dbname", value = cfg$dbname)
      updateTextInput(session, "user",   value = cfg$user)
    })
    
    cfg_reactive <- reactive({
      list(host=input$host, port=input$port, dbname=input$dbname,
           user=input$user, password=input$password)
    })
    
    observeEvent(input$test, {
      cfg <- cfg_reactive()
      res <- db_test_connection(cfg)
      output$conn_status <- renderUI(
        div(class = if (isTRUE(res$ok)) "text-success" else "text-danger",
            if (isTRUE(res$ok)) "Připojení OK" else paste("Chyba:", res$msg))
      )
      # check metadata existence
      try({
        con <- DBI::dbConnect(RPostgres::Postgres(),
                              host=cfg$host, port=as.integer(cfg$port),
                              dbname=cfg$dbname, user=cfg$user, password=cfg$password)
        on.exit(DBI::dbDisconnect(con), add=TRUE)
        has <- DBI::dbGetQuery(con,
                               "select exists(select 1 from information_schema.schemata where schema_name='app_meta') as ok")$ok
        if (isTRUE(has)) {
          output$meta_status <- renderUI(div(class="text-danger",
                                             "Metadata již existují v této DB – instalace je blokována."))
          shinyjs::disable(ns("init"))
        } else {
          output$meta_status <- renderUI(div(class="text-success",
                                             "Metadata nenalezena – lze instalovat."))
          shinyjs::enable(ns("init"))
        }
      }, silent = TRUE)
    })
    
    observeEvent(input$savecfg, {
      cfg <- cfg_reactive()
      save_app_config(cfg)
      get_db_pool(force_reload = TRUE)
      showNotification("Konfigurace uložena.")
    })
    
    observeEvent(input$init, {
      cfg <- cfg_reactive()
      con <- try(DBI::dbConnect(RPostgres::Postgres(),
                                host=cfg$host, port=as.integer(cfg$port),
                                dbname=cfg$dbname, user=cfg$user, password=cfg$password), silent=TRUE)
      if (inherits(con, "try-error")) { showNotification("Nelze se připojit k DB", type="error"); return() }
      on.exit(DBI::dbDisconnect(con), add=TRUE)
      
      has <- DBI::dbGetQuery(con,
                             "select exists(select 1 from information_schema.schemata where schema_name='app_meta') as ok")$ok
      if (isTRUE(has)) { showNotification("Metadata již existují – inicializace zablokována.", type="error"); return() }
      
      # načti externí SQL, jinak fallback (viz /sql/init_auth.sql)
      ddl <- NULL; path <- "sql/init_auth.sql"
      if (file.exists(path)) ddl <- readChar(path, file.info(path)$size)
      if (is.null(ddl)) {
        ddl <- paste(readLines(system.file(package = "base")), collapse = "\n") # dummy, přepíšeme hned…
        ddl <- paste0(
          "create schema if not exists app_meta;
create extension if not exists citext;

create table if not exists app_meta.user_account(
  user_id bigserial primary key,
  email citext not null unique,
  display_name text not null,
  password_hash text not null,
  is_active boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  last_login_at timestamptz,
  mfa_secret text
);

create table if not exists app_meta.role(
  role_id serial primary key,
  code text unique not null,
  title text not null
);

create table if not exists app_meta.user_role(
  user_id bigint references app_meta.user_account(user_id) on delete cascade,
  role_id int references app_meta.role(role_id) on delete cascade,
  primary key(user_id, role_id)
);

create table if not exists app_meta.session_log(
  session_id text primary key,
  user_id bigint references app_meta.user_account(user_id) on delete set null,
  login_at timestamptz not null default now(),
  logout_at timestamptz,
  ip inet,
  user_agent text
);

create or replace function app_meta.tg_updated_at() returns trigger
language plpgsql as $$
begin
  new.updated_at = now();
  return new;
end $$;

drop trigger if exists user_account_updated_at on app_meta.user_account;
create trigger user_account_updated_at
before update on app_meta.user_account
for each row execute function app_meta.tg_updated_at();

insert into app_meta.role(code,title) values
  ('admin','Administrátor'),('user','Uživatel')
on conflict do nothing;

create or replace view app_meta.v_user_roles as
select u.user_id, u.email, u.display_name, u.is_active,
       string_agg(r.code, ',') as roles
from app_meta.user_account u
left join app_meta.user_role ur on ur.user_id=u.user_id
left join app_meta.role r on r.role_id=ur.role_id
group by u.user_id, u.email, u.display_name, u.is_active;
")
      }
      
      run_ddl(con, ddl)
      showNotification("Metadata vytvořena.")
      output$meta_status <- renderUI(div(class="text-success",
                                         "Metadata vytvořena – nyní vytvořte admin účet."))
      shinyjs::disable(ns("init"))
    })
    
    observeEvent(input$mkadmin, {
      req(nzchar(input$admin_email), nzchar(input$admin_name), nzchar(input$admin_pw))
      shinyjs::disable("mkadmin")
      on.exit(shinyjs::enable("mkadmin"), add = TRUE)
      
      cfg <- list(host = input$host, port = input$port, dbname = input$dbname,
                  user = input$user, password = input$password)
      
      con <- try(DBI::dbConnect(RPostgres::Postgres(),
                                host = cfg$host, port = as.integer(cfg$port),
                                dbname = cfg$dbname, user = cfg$user, password = cfg$password),
                 silent = TRUE)
      if (inherits(con, "try-error")) {
        output$admin_status <- renderUI(div(class = "text-danger", "Nelze se připojit k DB."))
        return()
      }
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      
      has <- DBI::dbGetQuery(con,
                             "select exists(select 1 from information_schema.schemata where schema_name='app_meta') ok")$ok[[1]]
      if (!isTRUE(has)) {
        output$admin_status <- renderUI(div(class = "text-danger", "Metadata nejsou vytvořena."))
        return()
      }
      
      email <- tolower(trimws(input$admin_email))
      name  <- input$admin_name
      
      DBI::dbWithTransaction(con, {
        # Existuje už uživatel s tímto e-mailem?
        existing <- DBI::dbGetQuery(con,
                                    "select user_id from app_meta.user_account where email=$1",
                                    params = list(email))
        
        if (nrow(existing) > 0) {
          uid <- existing$user_id[1]
          # aktivuj účet a přiřaď role admin+user (bez změny hesla)
          DBI::dbExecute(con, "update app_meta.user_account set is_active=true where user_id=$1",
                         params = list(uid))
          DBI::dbExecute(con,
                         "insert into app_meta.user_role(user_id, role_id)
           select $1, role_id from app_meta.role where code in ('admin','user')
         on conflict do nothing",
                         params = list(uid))
          output$admin_status <- renderUI(div(
            class="text-warning",
            sprintf("Uživatel %s už existuje (user_id=%s). Přiřadil jsem mu roli admin a ponechal původní heslo.",
                    email, uid)
          ))
        } else {
          # nový admin (nastaví se zadané heslo)
          ph  <- sodium::password_store(input$admin_pw)
          uid <- DBI::dbGetQuery(con,
                                 "insert into app_meta.user_account(email, display_name, password_hash, is_active)
         values($1,$2,$3,true) returning user_id",
                                 params = list(email, name, ph))$user_id[[1]]
          
          DBI::dbExecute(con,
                         "insert into app_meta.user_role(user_id, role_id)
           select $1, role_id from app_meta.role where code in ('admin','user')
         on conflict do nothing",
                         params = list(uid))
          
          output$admin_status <- renderUI(div(
            class="text-success",
            sprintf("Admin vytvořen (user_id=%s).", uid)
          ))
          showNotification("Admin účet vytvořen.")
        }
      })
    })
    
  })
}
