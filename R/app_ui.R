#' Application UI
#'
#' Provides a compact skeleton interface with a consistent design based on
#' {bslib}. Public users can access the installation module, while secure
#' sections become available once authentication succeeds.
#'
#' @import shiny bslib shinyFeedback shinymanager
app_ui <- function(request) {
  theme <- bslib::bs_theme(version = 5, bootswatch = "flatly")

  page <- bslib::page_navbar(
    id = "main_nav",
    title = "RBudgeting",
    theme = theme,
    collapsible = TRUE,
    bslib::nav_panel("Instalace", value = "setup", mod_setup_ui("setup")),
    bslib::nav_panel("Obsah", value = "content", shiny::uiOutput("content_nav")),
    bslib::nav_panel("Uživatelé", value = "users", shiny::uiOutput("users_panel")),
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::div(class = "navbar-text", shiny::uiOutput("user_badge"))
    ),
    bslib::nav_item(shiny::uiOutput("auth_control"))
  )

  add_app_dependencies(
    shiny::tagList(
      shinyFeedback::useShinyFeedback(),
      page
    )
  )
}
