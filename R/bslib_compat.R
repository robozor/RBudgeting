# Compatibility helpers for bslib API changes ----

#' Wrapper around `bslib::badge()` with graceful degradation
#'
#' Older releases of bslib shipped without the `badge()` helper. When running
#' against such versions we still want to display a visual indicator instead of
#' failing with "not an exported object" errors. This helper checks whether the
#' package provides the modern implementation and, if not, renders a basic
#' Bootstrap compatible badge using `shiny::tags$span()`.
bslib_badge <- function(...) {
  badge_fun <- find_bslib_function("badge")
  if (!is.null(badge_fun)) {
    return(badge_fun(...))
  }

  args <- list(...)

  label <- NULL
  arg_names <- names(args)
  if ("label" %in% arg_names) {
    label <- args$label
    args$label <- NULL
  } else if ("text" %in% arg_names) {
    label <- args$text
    args$text <- NULL
  } else if (length(args) >= 1) {
    first_name <- if (length(arg_names) >= 1) arg_names[[1]] else NULL
    if (is.null(first_name) || first_name == "") {
      label <- args[[1]]
      args[[1]] <- NULL
    }
  }

  color <- args$color %||% NULL
  args$color <- NULL

  classes <- c("badge")
  if (!is.null(color)) {
    classes <- c(classes, paste0("bg-", color))
  }

  if ("class" %in% names(args) && !is.null(args$class)) {
    classes <- c(classes, args$class)
  }
  args$class <- paste(unique(classes), collapse = " ")

  children <- if (!is.null(label)) list(label) else list()

  do.call(shiny::tags$span, c(children, args))
}

find_bslib_function <- function(name) {
  if (!requireNamespace("bslib", quietly = TRUE)) {
    return(NULL)
  }

  exports <- tryCatch(utils::getNamespaceExports("bslib"), error = function(...) character())
  if (name %in% exports) {
    return(getExportedValue("bslib", name))
  }

  ns <- tryCatch(getNamespace("bslib"), error = function(...) NULL)
  if (!is.null(ns) && exists(name, envir = ns, inherits = FALSE)) {
    obj <- get(name, envir = ns, inherits = FALSE)
    if (is.function(obj)) {
      return(obj)
    }
  }

  NULL
}
