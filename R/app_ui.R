#' Application UI
#'
#' Defines the UI layout using bs4Dash components secured by shinymanager.
#' @import shiny bs4Dash shinyWidgets shinymanager
app_ui <- function(request) {
  sidebar <- bs4Dash::bs4DashSidebar(
    skin = "light",
    title = "Navigation",
    collapsed = FALSE,
    bs4Dash::bs4SidebarMenu(
      bs4Dash::bs4SidebarMenuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = shiny::icon("tachometer-alt")
      ),
      bs4Dash::bs4SidebarMenuItem(
        text = "Users",
        tabName = "users",
        icon = shiny::icon("users")
      ),
      bs4Dash::bs4SidebarMenuItem(
        text = "Setup",
        tabName = "setup",
        icon = shiny::icon("tools")
      )
    )
  )

  page_args <- list(
    title = "RBudgeting",
    freshTheme = NULL,
    preloader = list(html = NULL, color = "#3c8dbc"),
    navbar = bs4Dash::bs4DashNavbar(
      title = shiny::tags$span("RBudgeting"),
      skin = "dark",
      rightUi = shiny::tagList(
        bs4Dash::dropdownMenuOutput("notifications"),
        shinyWidgets::materialSwitch(
          inputId = "theme_toggle",
          label = "Dark mode",
          status = "primary"
        ),
        shiny::actionButton("logout", label = "Logout", icon = shiny::icon("sign-out-alt"))
      ),
      controlbarIcon = shiny::icon("sliders-h")
    ),
    sidebar = sidebar,
    controlbar = bs4Dash::bs4DashControlbar(
      id = "controlbar",
      skin = "dark",
      title = "Theme",
      shiny::tagList(
        shiny::tags$p("Select theme variant"),
        bs4Dash::skinSelector()
      )
    ),
    footer = bs4Dash::bs4DashFooter(
      left = "RBudgeting",
      right = shiny::HTML("<b>Version</b> 0.0.1")
    ),
    body = bs4Dash::bs4DashBody(
      shinyjs::useShinyjs(),
      shinyFeedback::useShinyFeedback(),
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "dashboard",
          bs4Dash::bs4Jumbotron(
            title = "Welcome",
            lead = "This is the RBudgeting dashboard skeleton."
          )
        ),
        shinydashboard::tabItem(
          tabName = "users",
          mod_user_management_ui("user_management")
        ),
        shinydashboard::tabItem(
          tabName = "setup",
          mod_setup_ui("setup")
        )
      )
    )
  )

  available_args <- names(formals(bs4Dash::bs4DashPage))

  if (!"navbar" %in% available_args && "header" %in% available_args && "navbar" %in% names(page_args)) {
    page_args$header <- page_args$navbar
  }
  if (!"controlbar" %in% available_args && "rightsidebar" %in% available_args && "controlbar" %in% names(page_args)) {
    page_args$rightsidebar <- page_args$controlbar
  }

  page_args <- page_args[names(page_args) %in% available_args]
  ui <- do.call(bs4Dash::bs4DashPage, page_args)

  shinymanager::secure_app(
    ui = add_app_dependencies(ui),
    language = get_golem_config()$app$language,
    enable_admin = TRUE,
    tags_top = shiny::tags$div(
      shiny::tags$h2("RBudgeting"),
      shiny::tags$p("Secure authentication provided by shinymanager.")
    ),
    theme = shinymanager::create_custom_theme(
      color = "#1f2d3d",
      hover = "#3c8dbc"
    )
  )
}
