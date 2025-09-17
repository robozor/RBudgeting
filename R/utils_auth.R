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
      return(default)
    }

    coerced <- tryCatch(as.character(value), error = function(e) character())
    if (length(coerced) == 0 || is.na(coerced[1]) || !nzchar(coerced[1])) {
      default
    } else {
      coerced[1]
    }
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
    conn <- conn_reactive()
    if (is.null(conn) || !DBI::dbIsValid(conn)) {
      return(failure_response("Database connection unavailable.", user))
    }

    record <- db_get_user(conn, user)
    if (is.null(record)) {
      return(failure_response("Unknown user", user))
    }

    if (!isTRUE(record$is_active)) {
      return(failure_response("Account disabled", user))
    }

    valid <- shinymanager::check_password(record$password, password)
    if (!isTRUE(valid)) {
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
  }
}
