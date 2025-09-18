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
    c(
      "logoutServer",
      "logout_server",
      "logout_module_server",
      "logout_module",
      "module_logout_server"
    )
  )
  if (!is.null(fun)) {
    return(fun(...))
  }

  shinymanager_logout_module_fallback(...)
}

#' Invoke shinymanager authentication module using a compatible API
#'
#' shinymanager has changed the calling conventions for `auth_server` across
#' releases. Some versions expect to be used through `shiny::callModule` with
#' the `input`, `output`, and `session` triple, while newer releases expose a
#' `moduleServer` wrapper taking `id` as the first argument. This helper
#' inspects the available signature and chooses the appropriate invocation so
#' that the rest of the application can remain agnostic of those differences.
#'
#' @param id Module identifier matching the UI call to `auth_ui`.
#' @param session Current shiny session (used when required by legacy APIs).
#' @param ... Additional arguments forwarded to the underlying implementation.
shinymanager_auth_server <- function(id, session, ...) {
  fun <- find_shinymanager_function(c("auth_server", "authServer"))

  if (is.null(fun)) {
    stop("No shinymanager authentication server function available.")
  }

  formals_names <- names(formals(fun))

  if (length(formals_names) >= 3 && identical(formals_names[1:3], c("input", "output", "session"))) {
    return(shiny::callModule(fun, id, ...))
  }

  if (length(formals_names) >= 1 && identical(formals_names[1], "id")) {
    args <- list(...)
    args$id <- id
    return(do.call(fun, args))
  }

  args <- list(...)
  if ("session" %in% formals_names && !("session" %in% names(args))) {
    args$session <- session
  }

  do.call(fun, args)
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

  ns <- tryCatch(getNamespace("shinymanager"), error = function(...) NULL)
  if (!is.null(ns)) {
    for (candidate in candidates) {
      if (exists(candidate, envir = ns, inherits = FALSE)) {
        obj <- get(candidate, envir = ns, inherits = FALSE)
        if (is.function(obj)) {
          return(obj)
        }
      }
    }
  }

  NULL
}

shinymanager_logout_module_fallback <- function(id, active = shiny::reactive(TRUE), ...) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (!isTRUE(shiny::is.reactive(active))) {
        active <- shiny::reactive(active)
      }

      shiny::observeEvent(input$logout, {
        shiny::req(isTRUE(active()))
        logout_fun <- find_shinymanager_function(
          c("logout", "logout_app", "reset", "reset_auth", "set_logout")
        )
        if (!is.null(logout_fun)) {
          try(logout_fun(session = session), silent = TRUE)
        } else {
          session$reload()
        }
      }, ignoreInit = TRUE)
    }
  )
}
