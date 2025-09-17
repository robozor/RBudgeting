#' Access application files
#'
#' @param ... Path elements relative to the package root.
#' @return A file path.
app_sys <- function(...) {
  system.file(..., package = "RBudgeting")
}

#' Retrieve sanitized application settings
#'
#' Ensures configuration values coming from `golem-config.yml` are usable in the
#' UI and server layers without additional checks.
#'
#' @return A list with `language` and `default_theme` entries.
get_app_settings <- function() {
  cfg <- get_golem_config()
  settings <- cfg$app

  if (is.null(settings) || !is.list(settings)) {
    settings <- list()
  }

  language <- sanitize_scalar_character(settings$language, default = "en")

  default_theme <- sanitize_scalar_character(settings$default_theme, default = "light")
  allowed_themes <- c("light", "dark")
  if (!default_theme %in% allowed_themes) {
    default_theme <- "light"
  }

  list(language = language, default_theme = default_theme)
}

sanitize_scalar_character <- function(value, default = "", allow_empty = FALSE) {
  if (is.null(value) || is.function(value)) {
    return(default)
  }

  coerced <- tryCatch(as.character(value), error = function(e) character())
  if (length(coerced) == 0) {
    return(default)
  }

  candidate <- coerced[[1]]
  if (is.na(candidate)) {
    return(default)
  }

  candidate <- trimws(candidate)
  if (!allow_empty && !nzchar(candidate)) {
    return(default)
  }

  candidate
}

sanitize_scalar_integer <- function(value, default = NA_integer_, min = NULL, max = NULL) {
  if (is.null(value) || is.function(value)) {
    return(default)
  }

  coerced <- suppressWarnings(as.integer(value))
  if (length(coerced) == 0) {
    return(default)
  }

  candidate <- coerced[[1]]
  if (is.na(candidate)) {
    return(default)
  }

  if (!is.null(min) && candidate < min) {
    return(default)
  }
  if (!is.null(max) && candidate > max) {
    return(default)
  }

  candidate
}

#' Use bs4Dash dependencies across versions
#'
#' The exported helper for attaching bs4Dash assets changed between
#' package releases. This wrapper tries the available function without
#' triggering errors on older or newer versions.
use_bs4dash_dependencies <- function() {
  exported <- getNamespaceExports("bs4Dash")

  if ("useBs4Dash" %in% exported) {
    return(bs4Dash::useBs4Dash())
  }
  if ("use_bs4dash" %in% exported) {
    return(bs4Dash::use_bs4dash())
  }

  ns <- asNamespace("bs4Dash")
  if (exists("useBs4Dash", envir = ns, inherits = FALSE)) {
    return(get("useBs4Dash", envir = ns)())
  }
  if (exists("use_bs4dash", envir = ns, inherits = FALSE)) {
    return(get("use_bs4dash", envir = ns)())
  }

  shiny::tagList()
}

#' Add a notification to the navbar menu
#' 
#' @param session Shiny session object
#' @param text Notification text
#' @param status Status color (info, primary, danger, ...)
#' @param icon Font Awesome icon name
add_notification <- function(session, text, status = "info", icon = "info-circle") {
  if (is.null(session$userData$notifications)) {
    return(invisible(FALSE))
  }
  notif_rv <- session$userData$notifications
  current <- notif_rv()
  notif_rv(c(list(list(text = text, status = status, icon = icon)), current))
  invisible(TRUE)
}
