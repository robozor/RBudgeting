# Compatibility helpers for shinymanager API changes ----

#' Invoke shinymanager logout module in a version agnostic way
#'
#' Recent versions of shinymanager renamed the logout module from
#' `logoutServer` to `logout_server`. Older releases only provide the former
#' camelCase export. This helper looks for the available implementation at
#' runtime and invokes it with the supplied arguments.
#'
#' @param ... Arguments forwarded to the underlying shinymanager logout module.
shinymanager_logout_module <- function(...) {
  fun <- find_shinymanager_function(
    c("logoutServer", "logout_server", "logout_module_server")
  )
  if (is.null(fun)) {
    stop("Unable to locate a shinymanager logout module.", call. = FALSE)
  }
  fun(...)
}

find_shinymanager_function <- function(candidates) {
  if (!requireNamespace("shinymanager", quietly = TRUE)) {
    return(NULL)
  }

  exports <- tryCatch(utils::getNamespaceExports("shinymanager"), error = function(...) character())
  for (candidate in candidates) {
    if (candidate %in% exports) {
      return(getExportedValue("shinymanager", candidate))
    }
  }

  NULL
}
