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

#' Determine the path to the runtime application configuration file
#'
#' The path can be overridden via the `RBUDGETING_APP_CONFIG` environment
#' variable; otherwise the package copy shipped in `inst/app/app_config.yml`
#' is used. When neither is available, the file is created inside the current
#' working directory.
get_app_config_path <- function() {
  env_path <- Sys.getenv("RBUDGETING_APP_CONFIG", unset = NA_character_)
  if (!is.na(env_path) && nzchar(env_path)) {
    normalized <- tryCatch(
      normalizePath(env_path, winslash = "/", mustWork = FALSE),
      error = function(e) env_path
    )
    log_debug("get_app_config_path.env", "Using path '", normalized, "'.")
    return(normalized)
  }

  pkg_path <- app_sys("app", "app_config.yml")
  if (!is.null(pkg_path) && nzchar(pkg_path)) {
    log_debug("get_app_config_path.pkg", "Using package path '", pkg_path, "'.")
    return(pkg_path)
  }

  fallback <- file.path(getwd(), "app_config.yml")
  log_debug("get_app_config_path.fallback", "Using fallback path '", fallback, "'.")
  fallback
}

#' Internal helper to sanitise a database configuration list
#' @noRd
sanitize_db_configuration <- function(db) {
  if (is.null(db) || !is.list(db)) {
    db <- list()
  }

  log_structure("sanitize_db_configuration.input", db)

  host <- sanitize_scalar_character(db$host, default = "localhost")
  port <- sanitize_scalar_integer(db$port, default = 5432L, min = 1L, max = 65535L)
  if (is.na(port)) {
    log_debug("sanitize_db_configuration", "Port sanitization returned NA; fallback to 5432.")
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
  log_structure("sanitize_db_configuration.output", sanitized)
  sanitized
}

#' Read runtime database configuration from disk
#' @noRd
load_persisted_db_config <- function() {
  path <- get_app_config_path()
  if (!file.exists(path)) {
    log_debug("load_persisted_db_config", "Configuration file '", path, "' not found.")
    return(NULL)
  }

  tryCatch(
    {
      cfg <- yaml::read_yaml(path, eval.expr = FALSE)
      log_structure("load_persisted_db_config.raw", cfg)
      if (is.list(cfg$db)) {
        return(cfg$db)
      }
      cfg
    },
    error = function(e) {
      log_debug(
        "load_persisted_db_config", "Unable to read configuration '", path, "': ", conditionMessage(e)
      )
      NULL
    }
  )
}

#' Persist database configuration to disk
#' @noRd
save_db_config <- function(db_cfg) {
  sanitized <- sanitize_db_configuration(db_cfg)
  path <- get_app_config_path()

  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    ok <- dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop(sprintf("Unable to create configuration directory '%s'.", dir_path))
    }
  }

  log_structure("save_db_config.sanitized", sanitized)

  tryCatch(
    {
      yaml::write_yaml(list(db = sanitized), path)
      log_debug("save_db_config", "Configuration written to '", path, "'.")
      invisible(sanitized)
    },
    error = function(e) {
      stop(sprintf("Unable to write configuration file '%s': %s", path, conditionMessage(e)))
    }
  )
}

#' Read database configuration
#'
#' @return A list containing database connection parameters.
get_db_config <- function() {
  defaults <- get_golem_config()
  db <- defaults$db

  if (is.null(db) || !is.list(db)) {
    log_debug("get_db_config", "Database configuration missing or invalid in golem-config; using defaults.")
    db <- list()
  }

  stored <- load_persisted_db_config()
  if (is.list(stored) && length(stored) > 0) {
    log_structure("get_db_config.persisted", stored)
    db <- utils::modifyList(db, stored)
  }

  sanitized <- sanitize_db_configuration(db)
  log_structure("get_db_config.final", sanitized)
  sanitized
}
