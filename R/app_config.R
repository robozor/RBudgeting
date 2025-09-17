#' Retrieve application configuration
#'
#' @return A list of configuration values coming from `golem-config.yml`.
get_golem_config <- function(config = c("default", "production")) {
  config <- match.arg(config)
  resolved <- config::get(
    config = config,
    file = app_sys("golem-config.yml"),
    use_parent = FALSE
  )
  log_structure("get_golem_config.result", resolved)
  resolved
}

#' Read database configuration
#'
#' @return A list containing database connection parameters.
get_db_config <- function() {
  cfg <- get_golem_config()
  db <- cfg$db

  if (is.null(db) || !is.list(db)) {
    log_debug("get_db_config", "Database configuration missing or invalid, using defaults.")
    db <- list()
  }

  log_structure("get_db_config.raw", db)

  host <- sanitize_scalar_character(db$host, default = "localhost")
  port <- sanitize_scalar_integer(db$port, default = 5432L, min = 1L, max = 65535L)
  if (is.na(port)) {
    log_debug("get_db_config", "Port sanitization returned NA; fallback to 5432.")
    port <- 5432L
  }

  dbname <- sanitize_scalar_character(db$dbname, default = "rbudgeting")
  user <- sanitize_scalar_character(db$user, default = "")
  password <- sanitize_scalar_character(db$password, default = "", allow_empty = TRUE)

  sslmode <- sanitize_scalar_character(db$sslmode, default = "prefer")
  allowed_ssl <- c("disable", "allow", "prefer", "require", "verify-ca", "verify-full")
  if (!sslmode %in% allowed_ssl) {
    sslmode <- "prefer"
  }

  sanitized <- list(
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password,
    sslmode = sslmode
  )
  log_structure("get_db_config.sanitized", sanitized)
  sanitized
}
