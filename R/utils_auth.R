#' Authentication helpers
#'
#' Provides wrappers around shinymanager to integrate with PostgreSQL storage.

#' Build a shinymanager credential checker using the database
#' @param conn_reactive A reactive expression returning a DBI connection.
#' @return A function compatible with `shinymanager::secure_server`.
credential_checker <- function(conn_reactive) {
  force(conn_reactive)
  function(user, password) {
    conn <- conn_reactive()
    if (is.null(conn) || !DBI::dbIsValid(conn)) {
      return(list(result = FALSE, message = "Database connection unavailable."))
    }
    record <- db_get_user(conn, user)
    if (is.null(record)) {
      return(FALSE)
    }
    if (!isTRUE(record$is_active)) {
      return(list(result = FALSE, message = "Account disabled"))
    }
    valid <- shinymanager::check_password(record$password, password)
    if (!isTRUE(valid)) {
      return(FALSE)
    }
    list(
      result = TRUE,
      user_info = list(
        user = record$username,
        fullname = record$fullname %||% record$username,
        role = record$role
      )
    )
  }
}
