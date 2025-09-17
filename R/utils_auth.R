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
      return(list(result = FALSE, message = "Unknown user"))
    }

    if (!isTRUE(record$is_active)) {
      return(list(result = FALSE, message = "Account disabled"))
    }

    valid <- shinymanager::check_password(record$password, password)
    if (!isTRUE(valid)) {
      return(list(result = FALSE, message = "Invalid credentials"))
    }

    fullname <- record$fullname
    if (!is.character(fullname) || length(fullname) == 0 || is.na(fullname) || !nzchar(fullname)) {
      fullname <- record$username
    }

    # V aplikaci se shinymanager snažil převést návratový objekt na text a narazil na closure,
    # protože jsme neposkytli očekávané textové pole `user`. Přidáním explicitní hodnoty
    # uživatele a doplňkových metadat vracíme strukturu, se kterou umí balíček správně pracovat.
    list(
      result = TRUE,
      user = as.character(record$username),
      admin = identical(as.character(record$role), "admin"),
      expire = NA,
      token = sprintf("rbudgeting-%s-%s", record$username, as.integer(Sys.time())),
      info = list(
        id = record$id,
        fullname = fullname,
        role = as.character(record$role)
      )
    )
  }
}
