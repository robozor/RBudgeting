#' Authentication helpers
#'
#' Provides wrappers around shinymanager to integrate with PostgreSQL storage.

#' Build a shinymanager credential checker using the database
#' @param conn_reactive A reactive expression returning a DBI connection.
#' @return A function compatible with `shinymanager::secure_server`.
credential_checker <- function(conn_reactive) {
  force(conn_reactive)

  sanitize_field <- function(value, default = "") {
    if (missing(value) || is.null(value)) {
      log_debug(
        "credential_checker.sanitize_field",
        "Missing or NULL value; using default '",
        default,
        "'."
      )
      return(default)
    }

    if (is.function(value)) {
      log_debug(
        "credential_checker.sanitize_field",
        "Function supplied; using default '",
        default,
        "'."
      )
      return(default)
    }

    coerced <- tryCatch(
      as.character(value),
      error = function(e) {
        log_debug(
          "credential_checker.sanitize_field",
          "Failed to coerce value: ",
          conditionMessage(e),
          ". Using default '",
          default,
          "'."
        )
        character()
      }
    )

    if (length(coerced) == 0) {
      log_debug(
        "credential_checker.sanitize_field",
        "Coercion produced empty result; using default '",
        default,
        "'."
      )
      return(default)
    }

    candidate <- coerced[1]

    if (is.na(candidate) || (!nzchar(candidate) && nzchar(default))) {
      log_debug(
        "credential_checker.sanitize_field",
        "Coercion result unusable; using default '",
        default,
        "'."
      )
      return(default)
    }

    log_debug(
      "credential_checker.sanitize_field",
      "Returning sanitized value '",
      candidate,
      "'."
    )
    candidate
  }

  failure_response <- function(message, attempted_user) {
    list(
      result = FALSE,
      message = message,
      user = sanitize_field(attempted_user),
      admin = FALSE,
      expire = NA,
      token = NULL,
      info = list()
    )
  }

  function(user, password) {
    tryCatch(
      {
        conn <- conn_reactive()
        if (is.null(conn) || !inherits(conn, "DBIConnection") || !DBI::dbIsValid(conn)) {
          return(failure_response("Database connection unavailable.", user))
        }

        log_debug("credential_checker", "Authenticating user '", user, "'.")

        record <- db_get_user(conn, user)
        if (is.null(record)) {
          log_debug("credential_checker", "User '", user, "' not found in database.")
          return(failure_response("Unknown user", user))
        }

        if (!isTRUE(record$is_active)) {
          log_debug("credential_checker", "User '", user, "' is inactive.")
          return(failure_response("Account disabled", user))
        }

        valid <- shinymanager::check_password(record$password, password)
        if (!isTRUE(valid)) {
          log_debug("credential_checker", "Invalid password provided for user '", user, "'.")
          return(failure_response("Invalid credentials", user))
        }

        username <- sanitize_field(record$username, default = sanitize_field(user))
        role <- sanitize_field(record$role, default = "user")

        fullname <- sanitize_field(record$fullname, default = username)

        list(
          result = TRUE,
          user = username,
          admin = identical(role, "admin"),
          expire = NA,
          token = sprintf("rbudgeting-%s-%s", username, as.integer(Sys.time())),
          info = list(
            id = record$id,
            fullname = fullname,
            role = role
          )
        )
      },
      error = function(e) {
        log_debug(
          "credential_checker",
          "Unexpected error while validating credentials: ",
          conditionMessage(e)
        )
        failure_response("Authentication service unavailable.", user)
      }
    )
  }
}
