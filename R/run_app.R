#' Run the RBudgeting Shiny application
#'
#' @param ... Additional parameters passed to `shiny::runApp`
#' @export
run_app <- function(...) {
  ui <- getFromNamespace("app_ui", "RBudgeting")
  server <- getFromNamespace("app_server", "RBudgeting")

  golem::with_golem_options(
    app = shiny::shinyApp(ui = ui, server = server),
    golem_opts = list(...)
  )
}
