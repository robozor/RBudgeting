#' Access application files
#'
#' @param ... Path elements relative to the package root.
#' @return A file path.
app_sys <- function(...) {
  system.file(..., package = "RBudgeting")
}

log_debug <- function(tag, ...) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  tag_formatted <- format_scalar_for_log(tag, fallback = "<missing_tag>")
  fragments <- list(...)

  if (length(fragments) > 0) {
    fragments <- vapply(
      fragments,
      format_scalar_for_log,
      character(1),
      fallback = "<empty>"
    )
    message(sprintf("[%s] [%s] %s", timestamp, tag_formatted, paste(fragments, collapse = "")))
  } else {
    message(sprintf("[%s] [%s]", timestamp, tag_formatted))
  }

  invisible(NULL)
}

log_structure <- function(tag, value) {
  header <- sprintf("[%s] structure:", format_scalar_for_log(tag, fallback = "<missing_tag>"))
  body <- utils::capture.output(utils::str(value, give.attr = FALSE))
  message(paste(c(header, body), collapse = "\n"))
  invisible(NULL)
}

redact_sensitive <- function(value, fields = c("password")) {
  if (!is.list(value)) {
    return(value)
  }

  fields <- tolower(fields)
  masked <- value
  nms <- names(masked)

  for (idx in seq_along(masked)) {
    current_name <- if (!is.null(nms)) nms[[idx]] else NULL

    if (!is.null(current_name) && tolower(current_name) %in% fields) {
      masked[[idx]] <- "<redacted>"
      next
    }

    if (is.list(masked[[idx]])) {
      masked[[idx]] <- redact_sensitive(masked[[idx]], fields)
    }
  }

  masked
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

  list(language = language, default_theme = default_theme)
}

sanitize_scalar_character <- function(value, default = "", allow_empty = FALSE, sensitive = FALSE) {
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

  if (!sensitive) {
    log_debug("sanitize_scalar_character", "Returning sanitized value '", candidate, "'.")
  }
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

#' Resolve Shiny UI candidates into renderable tags
#'
#' Some helpers (notably `shinymanager::secure_app`) return functions that must
#' be executed with the incoming HTTP request in order to obtain the HTML
#' markup expected by Shiny. This utility normalises such return values into a
#' `shiny.tag` object, safeguarding against unexpected values while preserving
#' diagnostics in the log stream.
#'
#' @param ui_candidate A UI definition, tag list or function.
#' @param request The active Shiny request environment, if available.
#' @param context A short label used in log messages to identify the caller.
#' @return A `shiny.tag` (or tag list) ready to be inserted into the UI tree.
resolve_shiny_ui <- function(ui_candidate, request = NULL, context = "ui") {
  if (!is.function(ui_candidate)) {
    return(ui_candidate)
  }

  fn <- ui_candidate
  fn_formals <- formals(fn)

  call_args <- list()
  if (length(fn_formals) > 0) {
    first_arg <- names(fn_formals)[[1]]
    call_args[[first_arg]] <- request
  }

  resolved <- tryCatch(
    do.call(fn, call_args),
    error = function(e) {
      log_debug(
        "resolve_shiny_ui",
        "Unable to resolve UI for context '",
        context,
        "': ",
        conditionMessage(e),
        "."
      )
      NULL
    }
  )

  if (is.null(resolved)) {
    log_debug(
      "resolve_shiny_ui",
      "Context '",
      context,
      "' produced NULL; substituting empty tagList."
    )
    return(shiny::tagList())
  }

  resolved
}

#' Use bs4Dash dependencies across versions
#'
#' The exported helper for attaching bs4Dash assets changed between
#' package releases. This wrapper tries the available function without
#' triggering errors on older or newer versions.
use_bs4dash_dependencies <- function() {
  if (!requireNamespace("bs4Dash", quietly = TRUE)) {
    log_debug(
      "use_bs4dash_dependencies",
      "bs4Dash namespace unavailable; returning empty tagList."
    )
    return(shiny::tagList())
  }

  ns <- asNamespace("bs4Dash")
  helper_candidates <- c("useBs4Dash", "use_bs4Dash", "use_bs4dash")

  for (helper_name in helper_candidates) {
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
