#' Run the RBudgeting Shiny application
#'
#' @param ... Additional parameters passed to `shiny::runApp`
#' @export
run_app <- function(...) {
  log_info <- function(...) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message("[run_app] ", timestamp, " - ", paste(..., collapse = ""))
  }

  log_info("Starting application bootstrap.")
  on.exit(log_info("run_app execution finished."), add = TRUE)

  dots <- list(...)
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("<unnamed>", length(dots))
  }
  log_info("Received ", length(dots), " additional argument(s).")
  if (length(dots) > 0) {
    log_info("Additional argument names: ", paste(dot_names, collapse = ", "))
    log_info("Additional argument summary:\n", paste(capture.output(str(dots, max.level = 1)), collapse = "\n"))
  }

  ui_name <- "app_ui"
  log_info("Retrieving UI constructor '", ui_name, "' from namespace RBudgeting.")
  ui <- getFromNamespace(ui_name, "RBudgeting")
  log_info(
    "UI constructor class: ", paste(class(ui), collapse = ", "),
    "; environment: ", environmentName(environment(ui))
  )

  server_name <- "app_server"
  log_info("Retrieving server function '", server_name, "' from namespace RBudgeting.")
  server <- getFromNamespace(server_name, "RBudgeting")
  log_info(
    "Server function class: ", paste(class(server), collapse = ", "),
    "; environment: ", environmentName(environment(server))
  )

  log_info("Preparing golem options and shiny application object.")
  golem_options <- list(...)
  log_info(
    "golem options summary:\n",
    paste(capture.output(str(golem_options, max.level = 1)), collapse = "\n")
  )

  shiny_app <- shiny::shinyApp(ui = ui, server = server)
  log_info(
    "Shiny app object constructed. Class: ",
    paste(class(shiny_app), collapse = ", ")
  )

  log_info("Launching application with golem::with_golem_options().")
  golem::with_golem_options(
    app = shiny_app,
    golem_opts = golem_options
  )
}
