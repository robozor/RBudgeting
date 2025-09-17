#' Application UI
#'
#' Defines the UI layout using bs4Dash components secured by shinymanager.
#' @import shiny bs4Dash shinyWidgets shinymanager
app_ui <- function(request) {
  app_settings <- get_app_settings()
  message(
    sprintf(
      "[app_ui] Loaded settings -> language: '%s', default_theme: '%s'",
      app_settings$language,
      app_settings$default_theme
    )
  )

  sidebar <- bs4Dash::bs4DashSidebar(
    skin = "light",
    title = "Navigation",
    collapsed = FALSE,
    class = "secure-hidden",
    bs4Dash::bs4SidebarMenu(
      id = "main_nav",
      bs4Dash::bs4SidebarMenuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = shiny::icon("tachometer-alt")
      ),
      bs4Dash::bs4SidebarMenuItem(
        text = "Users",
        tabName = "users",
        icon = shiny::icon("users")
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
        shiny::tags$li(
          class = "nav-item dropdown",
          bs4Dash::dropdownMenuOutput("notifications")
        ),
        shiny::tags$li(
          class = "nav-item dropdown",
          shinyWidgets::materialSwitch(
            inputId = "theme_toggle",
            label = "Dark mode",
            status = "primary"
          )
        ),
        shiny::tags$li(
          class = "nav-item dropdown",
          shiny::actionButton(
            "logout",
            label = "Logout",
            icon = shiny::icon("sign-out-alt")
          )
        )
      ),
      controlbarIcon = shiny::icon("sliders-h")
    ),
    sidebar = sidebar,
    controlbar = bs4Dash::bs4DashControlbar(
      id = "controlbar",
      skin = "dark",
      class = "secure-hidden",
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
  secure_ui <- do.call(bs4Dash::bs4DashPage, page_args)

  setup_header <- shiny::div(
    class = "setup-header",
    shiny::tags$h1("RBudgeting setup"),
    shiny::tags$p("Configure the database before enabling secure access.")
  )

  setup_public <- shiny::div(
    id = "public-setup",
    class = "setup-public-container",
    use_bs4dash_dependencies(),
    setup_header,
    mod_setup_ui("setup")
  )

  visibility_styles <- shiny::tags$head(
    shiny::tags$style(
      htmltools::HTML(
        paste0(
          "#secure-content.secure-hidden { display: none !important; }\n",
          "#public-setup.public-hidden { display: none !important; }"
        )
      )
    )
  )

  add_app_dependencies(
    shiny::tagList(
      shinyjs::useShinyjs(),
      shinyFeedback::useShinyFeedback(),
      visibility_styles,
      setup_public,
      shiny::div(
        id = "secure-content",
        class = "secure-hidden",
        shinymanager::secure_app(
          ui = secure_ui,
          language = app_settings$language,
          enable_admin = TRUE,
          tags_top = shiny::tags$div(
            shiny::tags$h2("RBudgeting"),
            shiny::tags$p("Secure authentication provided by shinymanager.")
          ),
          theme = NULL
        )
      )
    )
  )
}
