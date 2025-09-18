#' Database utilities for RBudgeting
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbIsValid
#' @importFrom RPostgres Postgres
#' @importFrom dplyr as_tibble

#' Connect to PostgreSQL using configuration
#' @param cfg Named list containing host, port, dbname, user, password, sslmode.
#' @return A DBI connection.
db_connect <- function(cfg) {
  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    host = cfg$host,
    port = cfg$port,
    dbname = cfg$dbname,
    user = cfg$user,
    password = cfg$password,
    sslmode = cfg$sslmode %||% "prefer"
  )
}

#' Disconnect helper
#' @param conn DBI connection
#' @return Invisible NULL
#' @noRd
db_disconnect <- function(conn) {
  if (!is.null(conn) && inherits(conn, "PqConnection")) {
    try({
      if (DBI::dbIsValid(conn)) {
        DBI::dbDisconnect(conn)
      }
    }, silent = TRUE)
  }
  invisible(NULL)
}

#' Install the application schema
#' @param conn Active DBI connection
#' @return TRUE when successful
#' @export
db_install_schema <- function(conn) {
  stopifnot(DBI::dbIsValid(conn))
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS app_users (\n    id SERIAL PRIMARY KEY,\n    username TEXT NOT NULL UNIQUE,\n    password TEXT NOT NULL,\n    fullname TEXT,\n    role TEXT NOT NULL DEFAULT 'user',\n    is_active BOOLEAN NOT NULL DEFAULT TRUE,\n    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),\n    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()\n  );")
  DBI::dbExecute(conn, "CREATE OR REPLACE FUNCTION trigger_set_timestamp()\nRETURNS TRIGGER AS $$\nBEGIN\n  NEW.updated_at = NOW();\n  RETURN NEW;\nEND;\n$$ LANGUAGE plpgsql;")
  DBI::dbExecute(conn, "DROP TRIGGER IF EXISTS set_timestamp ON app_users;")
  DBI::dbExecute(conn, "CREATE TRIGGER set_timestamp\nBEFORE UPDATE ON app_users\nFOR EACH ROW\nEXECUTE PROCEDURE trigger_set_timestamp();")
  TRUE
}

#' Create or update a user record
#' @param conn Active connection
#' @param username Unique username
#' @param password Plain text password
#' @param fullname Optional full name
#' @param role Role identifier
#' @param active Logical flag
#' @return TRUE when command executed
#' @export
db_upsert_user <- function(conn, username, password, fullname = NULL, role = "user", active = TRUE) {
  stopifnot(DBI::dbIsValid(conn))
  hashed <- hash_password_compat(password)
  query <- "INSERT INTO app_users (username, password, fullname, role, is_active)\n            VALUES ($1, $2, $3, $4, $5)\n            ON CONFLICT (username) DO UPDATE SET\n              password = EXCLUDED.password,\n              fullname = EXCLUDED.fullname,\n              role = EXCLUDED.role,\n              is_active = EXCLUDED.is_active,\n              updated_at = NOW();"
  DBI::dbExecute(conn, query, params = list(username, hashed, fullname, role, active))
  TRUE
}

#' Retrieve users
#' @param conn Active connection
#' @return A tibble of users without exposing hashed passwords.
db_get_users <- function(conn) {
  stopifnot(DBI::dbIsValid(conn))

  base <- DBI::dbGetQuery(
    conn,
    "SELECT id, username, fullname, role, is_active, created_at, updated_at FROM app_users ORDER BY username;"
  )

  base <- dplyr::as_tibble(base)

  metadata <- tryCatch(
    DBI::dbGetQuery(conn, "SELECT * FROM app_meta.user_account;"),
    error = function(e) NULL
  )

  if (is.null(metadata) || nrow(metadata) == 0) {
    return(base)
  }

  rename_first <- function(df, target, candidates) {
    for (candidate in candidates) {
      if (candidate %in% names(df)) {
        names(df)[names(df) == candidate] <- target
        break
      }
    }
    df
  }

  meta <- rename_first(metadata, "username", c("username", "user_name", "login", "login_name"))

  if (!"username" %in% names(meta)) {
    return(base)
  }

  meta$username <- trimws(as.character(meta$username))

  meta <- rename_first(meta, "fullname", c("fullname", "full_name", "name", "display_name"))
  meta <- rename_first(meta, "role", c("role", "role_name", "user_role"))
  meta <- rename_first(meta, "is_active", c("is_active", "active", "enabled", "is_enabled"))
  meta <- rename_first(meta, "created_at", c("created_at", "created", "created_on"))
  meta <- rename_first(meta, "updated_at", c("updated_at", "updated", "updated_on", "modified_at"))

  ensure_cols <- c("fullname", "role", "is_active", "created_at", "updated_at")
  for (nm in ensure_cols) {
    if (!nm %in% names(meta)) {
      meta[[nm]] <- NA
    }
  }

  if ("is_active" %in% names(meta)) {
    if (is.logical(meta$is_active)) {
      # nothing to do
    } else if (is.numeric(meta$is_active)) {
      meta$is_active <- meta$is_active != 0
    } else if (is.character(meta$is_active)) {
      normalized <- tolower(trimws(meta$is_active))
      meta$is_active <- normalized %in% c("1", "true", "t", "yes", "y", "enabled")
    } else {
      meta$is_active <- as.logical(meta$is_active)
    }
  }

  meta <- meta[!is.na(meta$username) & nzchar(meta$username), , drop = FALSE]

  if (!"id" %in% names(meta)) {
    meta$id <- as.integer(NA)
  }

  # Ensure we keep a single row per username to avoid duplication on join
  meta <- meta[order(meta$username), , drop = FALSE]
  meta <- meta[!duplicated(meta$username), , drop = FALSE]

  meta <- dplyr::as_tibble(meta)

  joined <- dplyr::left_join(
    base,
    meta,
    by = "username",
    suffix = c("", ".meta")
  )

  coalesce_cols <- function(primary, secondary) {
    if (!secondary %in% names(joined)) {
      return(joined[[primary]])
    }
    dplyr::coalesce(joined[[primary]], joined[[secondary]])
  }

  joined$fullname <- coalesce_cols("fullname", "fullname.meta")
  joined$role <- coalesce_cols("role", "role.meta")
  joined$is_active <- coalesce_cols("is_active", "is_active.meta")
  joined$created_at <- coalesce_cols("created_at", "created_at.meta")
  joined$updated_at <- coalesce_cols("updated_at", "updated_at.meta")

  joined <- dplyr::select(joined, -dplyr::ends_with(".meta"))

  joined
}

#' Retrieve a single user record including password hash
#' @param conn Active connection
#' @param username Username to search
#' @return A list or NULL
#' @noRd
db_get_user <- function(conn, username) {
  stopifnot(DBI::dbIsValid(conn))
  res <- DBI::dbGetQuery(conn, "SELECT * FROM app_users WHERE username = $1 LIMIT 1;", params = list(username))
  if (nrow(res) == 0) {
    return(NULL)
  }
  res[1, , drop = FALSE]
}

#' Toggle user activation
#' @param conn Active connection
#' @param username Username to change
#' @param active Logical flag
#' @return TRUE when update executed
db_set_user_active <- function(conn, username, active = TRUE) {
  stopifnot(DBI::dbIsValid(conn))
  DBI::dbExecute(conn, "UPDATE app_users SET is_active = $1 WHERE username = $2;", params = list(active, username))
  TRUE
}

#' Delete a user
#' @param conn Active connection
#' @param username Username to delete
#' @return TRUE when deleted
#' @export
db_delete_user <- function(conn, username) {
  stopifnot(DBI::dbIsValid(conn))
  DBI::dbExecute(conn, "DELETE FROM app_users WHERE username = $1;", params = list(username))
  TRUE
}

#' Ensure an admin user exists
#' @param conn Active connection
#' @param username Admin username
#' @param password Plain text password
#' @param fullname Optional name
#' @return TRUE when ensured
#' @export
db_ensure_admin <- function(conn, username, password, fullname = NULL) {
  stopifnot(DBI::dbIsValid(conn))
  existing <- db_get_user(conn, username)
  if (is.null(existing)) {
    db_upsert_user(conn, username, password, fullname = fullname, role = "admin", active = TRUE)
  }
  TRUE
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0 || is.na(lhs)) {
    rhs
  } else {
    lhs
  }
}

#' Hash a password using available shinymanager utilities
#' @param password Plain text password
#' @return A hashed password string
#' @noRd
hash_password_compat <- function(password) {
  sm_ns <- asNamespace("shinymanager")

  if (exists("hash_password", envir = sm_ns, inherits = FALSE)) {
    return(get("hash_password", envir = sm_ns)(password))
  }

  if (exists("encrypt_password", envir = sm_ns, inherits = FALSE)) {
    return(get("encrypt_password", envir = sm_ns)(password))
  }

  if (requireNamespace("sodium", quietly = TRUE)) {
    return(sodium::password_store(password))
  }

  stop("No available password hashing implementation found.")
}

#' Verify a password using available shinymanager utilities
#' @param hash Stored password hash
#' @param password Plain text password to validate
#' @return TRUE when password matches the stored hash
#' @noRd
check_password_compat <- function(hash, password) {
  sm_ns <- asNamespace("shinymanager")

  if (exists("check_password", envir = sm_ns, inherits = FALSE)) {
    return(get("check_password", envir = sm_ns)(hash, password))
  }

  if (exists("hash_password", envir = sm_ns, inherits = FALSE) &&
      requireNamespace("sodium", quietly = TRUE)) {
    return(sodium::password_verify(hash, password))
  }

  if (exists("encrypt_password", envir = sm_ns, inherits = FALSE)) {
    encrypt <- get("encrypt_password", envir = sm_ns)
    return(identical(hash, encrypt(password)))
  }

  if (requireNamespace("sodium", quietly = TRUE)) {
    return(sodium::password_verify(hash, password))
  }

  identical(hash, password)
}
