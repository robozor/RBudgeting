#' content Module UI
#'
#' @param id Module id
mod_content_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Obsah"),
    bslib::card_body(shiny::uiOutput(ns("body")))
  )
}

#' content Module Server
#'
#' @param id Module id
#' @param current_user Reactive expression returning the current user label
#' @param is_authenticated Reactive expression evaluating to TRUE when logged in
mod_content_server <- function(id, current_user, is_authenticated) {
  shiny::moduleServer(id, function(input, output, session) {
    output$body <- shiny::renderUI({
      if (!isTRUE(is_authenticated())) {
        shiny::div()
      } else {
        shiny::tagList(
          shiny::p("Aktuálně přihlášený uživatel:"),
          shiny::h4(current_user())
        )
      }
    })
  })
}
