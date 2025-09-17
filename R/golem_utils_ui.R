#' @import shiny
NULL

#' Append dependencies to the UI
#'
#' @param tag A tag or tagList to decorate.
#' @return The tag with dependencies.
add_app_dependencies <- function(tag) {
  golem::add_resource_path("www", app_sys("app/www"))
  shiny::tagList(
    golem::favicon(),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
      shiny::tags$script(src = "www/theme-toggle.js")
    ),
    tag
  )
}
