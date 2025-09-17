#' Access application files
#'
#' @param ... Path elements relative to the package root.
#' @return A file path.
app_sys <- function(...) {
  system.file(..., package = "RBudgeting")
}

log_debug <- function(tag, ...) {
  fragments <- lapply(list(...), function(piece) {
    if (is.null(piece)) {
      return("NULL")
    }
    if (is.character(piece)) {
      return(paste(piece, collapse = ""))
    }
    if (is.list(piece) || is.environment(piece)) {
      return(
        tryCatch(
          paste(capture.output(utils::str(piece)), collapse = " | "),
          error = function(e) sprintf("<unserializable: %s>", conditionMessage(e))
        )
      )
    }
    tryCatch(
      toString(piece),
      error = function(e) sprintf("<unserializable: %s>", conditionMessage(e))
    )
  })

  message(sprintf("[%s] %s", tag, paste(fragments, collapse = "")))
}

log_structure <- function(tag, value) {
  details <- tryCatch(
    paste(capture.output(utils::str(value)), collapse = " | "),
    error = function(e) sprintf("<unable to inspect: %s>", conditionMessage(e))
  )
  message(sprintf("[%s] %s", tag, details))
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
    log_debug("get_app_settings", "App configuration missing or invalid, using defaults.")
    settings <- list()
  }

  log_structure("get_app_settings.raw", settings)

  language <- sanitize_scalar_character(settings$language, default = "en")

  default_theme <- sanitize_scalar_character(settings$default_theme, default = "light")
  allowed_themes <- c("light", "dark")
  if (!default_theme %in% allowed_themes) {
    log_debug(
      "get_app_settings",
      "Unsupported default_theme received.",
      " Fallback to 'light'."
    )
    default_theme <- "light"
  }

  message(
    sprintf(
      "[get_app_settings] Sanitized values -> language: '%s', default_theme: '%s'",
      language,
      default_theme
    )
  )

  list(language = language, default_theme = default_theme)
}

sanitize_scalar_character <- function(value, default = "", allow_empty = FALSE) {
  if (is.null(value) || is.function(value)) {
    log_debug(
      "sanitize_scalar_character",
      "Received ",
      if (is.null(value)) "NULL" else "function",
      "; returning default '",
      default,
      "'."
    )
    return(default)
  }

  coerced <- tryCatch(
    as.character(value),
    error = function(e) {
      log_debug(
        "sanitize_scalar_character",
        "Coercion error: ",
        conditionMessage(e),
        ". Returning default '",
        default,
        "'."
      )
      character()
    }
  )
  if (length(coerced) == 0) {
    log_debug(
      "sanitize_scalar_character",
      "Coercion produced empty result; returning default '",
      default,
      "'."
    )
    return(default)
  }

  candidate <- coerced[[1]]
  if (is.na(candidate)) {
    log_debug(
      "sanitize_scalar_character",
      "First coerced value is NA; returning default '",
      default,
      "'."
    )
    return(default)
  }

  candidate <- trimws(candidate)
  if (!allow_empty && !nzchar(candidate)) {
    log_debug(
      "sanitize_scalar_character",
      "Trimmed value empty and empty not allowed; returning default '",
      default,
      "'."
    )
    return(default)
  }

  log_debug("sanitize_scalar_character", "Returning sanitized value '", candidate, "'.")
  candidate
}

sanitize_scalar_integer <- function(value, default = NA_integer_, min = NULL, max = NULL) {
  if (is.null(value) || is.function(value)) {
    log_debug(
      "sanitize_scalar_integer",
      "Received ",
      if (is.null(value)) "NULL" else "function",
      "; returning default '",
      default,
      "'."
    )
    return(default)
  }

  coerced <- suppressWarnings(as.integer(value))
  if (length(coerced) == 0) {
    log_debug(
      "sanitize_scalar_integer",
      "Coercion produced empty result; returning default '",
      default,
      "'."
    )
    return(default)
  }

  candidate <- coerced[[1]]
  if (is.na(candidate)) {
    log_debug(
      "sanitize_scalar_integer",
      "First coerced value is NA; returning default '",
      default,
      "'."
    )
    return(default)
  }

  if (!is.null(min) && candidate < min) {
    log_debug(
      "sanitize_scalar_integer",
      "Value ",
      candidate,
      " is below minimum ",
      min,
      "; returning default '",
      default,
      "'."
    )
    return(default)
  }
  if (!is.null(max) && candidate > max) {
    log_debug(
      "sanitize_scalar_integer",
      "Value ",
      candidate,
      " exceeds maximum ",
      max,
      "; returning default '",
      default,
      "'."
    )
    return(default)
  }

  log_debug("sanitize_scalar_integer", "Returning sanitized value ", candidate, ".")
  candidate
}

#' Safely format a scalar value for logging
#'
#' Converts arbitrary inputs into a character representation suitable for
#' concatenation in log messages while avoiding errors when functions or other
#' unsupported objects are supplied.
#'
#' @param value Value to format.
#' @param fallback String to use when the value cannot be represented.
#' @return A single character string.
format_scalar_for_log <- function(value, fallback = "<unavailable>") {
  if (is.null(value)) {
    return("NULL")
  }
  if (is.function(value)) {
    return("<function>")
  }

  coerced <- tryCatch(
    as.character(value),
    error = function(e) character()
  )

  if (length(coerced) == 0 || is.na(coerced[1])) {
    return(fallback)
  }

  coerced[[1]]
}

#' Use bs4Dash dependencies across versions
#'
#' The exported helper for attaching bs4Dash assets changed between
#' package releases. This wrapper tries the available function without
#' triggering errors on older or newer versions.
use_bs4dash_dependencies <- function() {
  if (!requireNamespace("bs4Dash", quietly = TRUE)) {
    log_debug("use_bs4dash_dependencies", "bs4Dash namespace unavailable; returning empty tagList.")
    return(shiny::tagList())
  }

  ns <- asNamespace("bs4Dash")
  helpers <- c("useBs4Dash", "use_bs4dash")

  for (helper_name in helpers) {
    if (!exists(helper_name, envir = ns, inherits = FALSE)) {
      next
    }

    helper <- get(helper_name, envir = ns)
    result <- tryCatch(
      if (is.function(helper)) helper() else helper,
      error = function(e) {
        log_debug(
          "use_bs4dash_dependencies",
          "Failed to invoke helper '",
          helper_name,
          "': ",
          conditionMessage(e),
          "."
        )
        NULL
      }
    )

    if (is.null(result)) {
      next
    }

    if (is.function(result)) {
      log_debug(
        "use_bs4dash_dependencies",
        "Helper '",
        helper_name,
        "' returned a function; ignoring unexpected value."
      )
      next
    }

    return(result)
  }

  log_debug(
    "use_bs4dash_dependencies",
    "No suitable helper found; returning empty tagList."
  )
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
