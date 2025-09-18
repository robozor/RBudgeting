#' Application UI
#'
#' Defines the UI layout using bs4Dash components secured by shinymanager.
#' @import shiny bs4Dash shinyWidgets shinymanager
app_ui <- function(request) {
  app_settings <- get_app_settings()
  log_structure("app_ui.settings", app_settings)
  message(
    sprintf(
      "[app_ui] Loaded settings -> language: '%s', default_theme: '%s'",
      app_settings$language,
      app_settings$default_theme
    )
  )

  sidebar <- bs4Dash::bs4DashSidebar(
    skin = "light",
    title = "Navigace",
    collapsed = FALSE,
    class = "secure-hidden",
    bs4Dash::bs4SidebarMenu(
      id = "main_nav",
      bs4Dash::bs4SidebarMenuItem(
        text = "Obsah",
        tabName = "content",
        icon = shiny::icon("home")
      ),
      bs4Dash::bs4SidebarMenuItem(
        text = "Administrace",
        icon = shiny::icon("cogs"),
        startExpanded = TRUE,
        bs4Dash::bs4SidebarMenuSubItem(
          text = "Instalace systému",
          tabName = "setup_admin",
          icon = shiny::icon("tools")
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          text = "Administrace uživatelů",
          tabName = "users",
          icon = shiny::icon("users")
        )
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
          class = "nav-item dropdown d-flex align-items-center px-3",
          shiny::icon("user-circle", class = "mr-2"),
          shiny::textOutput("navbar_user", container = shiny::span)
        ),
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
            label = "Odhlásit se",
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
          tabName = "content",
          bs4Dash::bs4Card(
            title = "Obsah",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            shiny::div(
              class = "text-center py-4",
              shiny::h3("Vítejte v RBudgeting"),
              shiny::p("Aktuálně přihlášený uživatel:"),
              shiny::h4(shiny::textOutput("content_user", container = shiny::span))
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "setup_admin",
          bs4Dash::bs4Card(
            title = "Instalace systému",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            mod_setup_ui("setup_admin")
          )
        ),
        shinydashboard::tabItem(
          tabName = "users",
          bs4Dash::bs4Card(
            title = "Administrace uživatelů",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            mod_user_management_ui("user_management")
          )
        )
      )
    )
  )

  available_args <- names(formals(bs4Dash::bs4DashPage))
  message(
    sprintf(
      "[app_ui] Detected %d available bs4Dash::bs4DashPage arguments.",
      length(available_args)
    )
  )
  log_structure("app_ui.available_args", available_args)

  if (!"navbar" %in% available_args && "header" %in% available_args && "navbar" %in% names(page_args)) {
    page_args$header <- page_args$navbar
  }
  if (!"controlbar" %in% available_args && "rightsidebar" %in% available_args && "controlbar" %in% names(page_args)) {
    page_args$rightsidebar <- page_args$controlbar
  }

  page_args <- page_args[names(page_args) %in% available_args]
  log_structure("app_ui.page_args", page_args)
  secure_ui <- tryCatch(
    {
      ui_obj <- do.call(bs4Dash::bs4DashPage, page_args)
      message(
        sprintf(
          "[app_ui] secure_ui constructed. Class: %s",
          paste(class(ui_obj), collapse = ", ")
        )
      )
      ui_obj
    },
    error = function(e) {
      message(
        sprintf(
          "[app_ui] Error constructing secure_ui: %s",
          conditionMessage(e)
        )
      )
      log_structure("app_ui.page_args_on_error", page_args)
      stop(e)
    }
  )

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
  message(
    sprintf(
      "[app_ui] setup_public constructed. Class: %s",
      paste(class(setup_public), collapse = ", ")
    )
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

  secure_wrapper <- shinymanager::secure_app(
    ui = secure_ui,
    language = app_settings$language,
    enable_admin = TRUE,
    tags_top = shiny::tags$div(
      shiny::tags$h2("RBudgeting"),
      shiny::tags$p("Secure authentication provided by shinymanager.")
    ),
    theme = NULL
  )

  secure_content <- resolve_shiny_ui(
    secure_wrapper,
    request = request,
    context = "app_ui.secure_app"
  )
  message(
    sprintf(
      "[app_ui] secure_content resolved. Class: %s",
      paste(class(secure_content), collapse = ", ")
    )
  )

  final_ui <- add_app_dependencies(
    shiny::tagList(
      shinyjs::useShinyjs(),
      shinyFeedback::useShinyFeedback(),
      visibility_styles,
      setup_public,
      shiny::div(
        id = "secure-content",
        class = "secure-hidden",
        secure_content
      )
    )
  )
  message(
    sprintf(
      "[app_ui] Final UI assembled. Class: %s",
      paste(class(final_ui), collapse = ", ")
    )
  )
  final_ui
}
