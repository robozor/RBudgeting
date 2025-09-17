#' Run the RBudgeting Shiny application
#'
#' @param ... Additional parameters passed to `shiny::runApp`
#' @export
run_app <- function(...) {
  format_for_log <- function(value) {
    if (inherits(value, "condition")) {
      return(
        sprintf(
          "<%s: %s>",
          paste(class(value), collapse = ","),
          conditionMessage(value)
        )
      )
    }

    if (is.null(value)) {
      return("NULL")
    }

    if (is.function(value)) {
      fn_env <- tryCatch(environmentName(environment(value)), error = function(e) "<unknown>")
      return(sprintf("<function:%s>", fn_env))
    }

    if (is.list(value) || is.environment(value)) {
      summary <- tryCatch(
        paste(capture.output(str(value, max.level = 1)), collapse = " | "),
        error = function(e) sprintf("<inspect failed: %s>", conditionMessage(e))
      )
      return(summary)
    }

    formatted <- tryCatch(
      format(value, trim = TRUE),
      error = function(e) character()
    )

    if (length(formatted) == 0) {
      return(format_scalar_for_log(value, fallback = "<unrepresentable>"))
    }

    paste(formatted, collapse = " ")
  }

  compose_log_message <- function(...) {
    parts <- vapply(list(...), format_for_log, character(1))
    paste(parts, collapse = "")
  }

  emit_log <- function(channel, ...) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(sprintf("[%s] %s - %s", channel, timestamp, compose_log_message(...)))
  }

  log_info <- function(...) {
    emit_log("run_app", ...)
  }

  current_stage <- "bootstrap"
  advance_stage <- function(stage) {
    current_stage <<- stage
    emit_log("run_app.stage", sprintf("Transition -> %s", stage))
  }

  capture_calls <- function(limit = 5) {
    tryCatch(
      {
        calls <- sys.calls()
        if (length(calls) == 0) {
          return("<no calls>")
        }
        tail_calls <- tail(calls, limit)
        collapsed <- vapply(
          tail_calls,
          function(call) paste(deparse(call, width.cutoff = 80), collapse = " "),
          character(1)
        )
        paste(collapsed, collapse = " <- ")
      },
      error = function(e) sprintf("<stack unavailable: %s>", conditionMessage(e))
    )
  }

  log_condition <- function(cond, source) {
    emit_log(
      sprintf("run_app.%s", source),
      sprintf("Stage='%s' | Class='%s' | Message='%s' | Call=%s | Stack=%s",
        current_stage,
        paste(class(cond), collapse = ","),
        conditionMessage(cond),
        if (!is.null(cond$call)) paste(deparse(cond$call, width.cutoff = 80), collapse = " ") else "<no call>",
        capture_calls()
      )
    )
  }

  log_info("Starting application bootstrap.")
  on.exit(log_info("run_app execution finished."), add = TRUE)

  previous_shiny_error <- getOption("shiny.error")
  previous_stacktrace <- getOption("shiny.fullstacktrace")
  options(shiny.fullstacktrace = TRUE)
  options(shiny.error = function(...) {
    args <- list(...)
    cond <- NULL
    if (length(args) > 0 && inherits(args[[1]], "condition")) {
      cond <- args[[1]]
    }
    if (!is.null(cond)) {
      log_condition(cond, "shiny.error")
    } else {
      emit_log("run_app.shiny.error", "Triggered without condition payload.")
    }

    if (!is.null(previous_shiny_error)) {
      try(do.call(previous_shiny_error, args), silent = TRUE)
    }
  })
  on.exit(options(shiny.error = previous_shiny_error), add = TRUE)
  on.exit(options(shiny.fullstacktrace = previous_stacktrace), add = TRUE)

  dots <- list(...)
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("<unnamed>", length(dots))
  }
  log_info("Received ", length(dots), " additional argument(s).")
  if (length(dots) > 0) {
    log_info("Additional argument names: ", paste(dot_names, collapse = ", "))
    log_info(
      "Additional argument summary:\n",
      paste(capture.output(str(dots, max.level = 1)), collapse = "\n")
    )
  }

  advance_stage("ui_lookup")
  ui_name <- "app_ui"
  log_info("Retrieving UI constructor '", ui_name, "' from namespace RBudgeting.")
  ui <- getFromNamespace(ui_name, "RBudgeting")
  log_info(
    "UI constructor class: ", paste(class(ui), collapse = ", "),
    "; environment: ", environmentName(environment(ui))
  )

  advance_stage("server_lookup")
  server_name <- "app_server"
  log_info("Retrieving server function '", server_name, "' from namespace RBudgeting.")
  server <- getFromNamespace(server_name, "RBudgeting")
  log_info(
    "Server function class: ", paste(class(server), collapse = ", "),
    "; environment: ", environmentName(environment(server))
  )

  advance_stage("app_construction")
  log_info("Preparing golem options and shiny application object.")
  golem_options <- list(...)
  log_info(
    "golem options summary:\n",
    paste(capture.output(str(golem_options, max.level = 1)), collapse = "\n")
  )

  shiny_app <- withCallingHandlers(
    {
      shiny::shinyApp(ui = ui, server = server)
    },
    warning = function(w) {
      log_condition(w, "warning")
    },
    error = function(e) {
      log_condition(e, "error")
      stop(e)
    }
  )
  log_info(
    "Shiny app object constructed. Class: ",
    paste(class(shiny_app), collapse = ", ")
  )

  advance_stage("runtime")
  log_info("Launching application with golem::with_golem_options().")
  withCallingHandlers(
    {
      golem::with_golem_options(
        app = shiny_app,
        golem_opts = golem_options
      )
    },
    warning = function(w) {
      log_condition(w, "warning")
    },
    error = function(e) {
      log_condition(e, "error")
      stop(e)
    }
  )
}
