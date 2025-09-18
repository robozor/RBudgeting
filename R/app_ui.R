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

  build_sidebar <- function(id, authenticated = FALSE, class = NULL) {
    setup_item <- bs4Dash::bs4SidebarMenuSubItem(
      text = "Instalace systému",
      tabName = "setup",
      icon = shiny::icon("tools")
    )

    users_item <- bs4Dash::bs4SidebarMenuSubItem(
      text = "Administrace uživatelů",
      tabName = "users",
      icon = shiny::icon("users")
    )

    if (!authenticated) {
      users_item <- shiny::tagAppendAttributes(users_item, class = "menu-item-disabled")
    }

    admin_menu <- bs4Dash::bs4SidebarMenuItem(
      text = "Administrace",
      icon = shiny::icon("cogs"),
      startExpanded = TRUE,
      setup_item,
      users_item
    )

    bs4Dash::bs4DashSidebar(
      skin = "light",
      title = "Navigace",
      collapsed = FALSE,
      class = class,
      bs4Dash::bs4SidebarMenu(
        id = id,
        bs4Dash::bs4SidebarMenuItem(
          text = "Obsah",
          tabName = "content",
          icon = shiny::icon("home")
        ),
        admin_menu
      )
    )
  }

  build_navbar <- function(authenticated = FALSE, show_notifications = FALSE, theme_input_id = "theme_toggle") {
    user_display <- if (authenticated) {
      shiny::textOutput("navbar_user", container = shiny::span)
    } else {
      shiny::span("Nepřihlášen")
    }

    auth_button <- if (authenticated) {
      shiny::actionButton(
        "logout",
        label = "Logout",
        icon = shiny::icon("sign-out-alt")
      )
    } else {
      shiny::actionButton(
        "login",
        label = "Login",
        icon = shiny::icon("sign-in-alt")
      )
    }

    right_ui <- shiny::tagList(
      shiny::tags$li(
        class = "nav-item dropdown d-flex align-items-center px-3",
        shiny::icon("user-circle", class = "mr-2"),
        user_display
      ),
      if (show_notifications) {
        shiny::tags$li(
          class = "nav-item dropdown",
          bs4Dash::dropdownMenuOutput("notifications")
        )
      },
      shiny::tags$li(
        class = "nav-item dropdown",
        shinyWidgets::materialSwitch(
          inputId = theme_input_id,
          label = "Dark mode",
          status = "primary"
        )
      ),
      shiny::tags$li(
        class = "nav-item dropdown",
        auth_button
      )
    )

    bs4Dash::bs4DashNavbar(
      title = shiny::tags$span("RBudgeting"),
      skin = "dark",
      rightUi = right_ui,
      controlbarIcon = shiny::icon("sliders-h")
    )
  }

  sidebar <- build_sidebar(
    id = "main_nav",
    authenticated = TRUE,
    class = "secure-hidden secure-sidebar"
  )

  page_args <- list(
    title = "RBudgeting",
    freshTheme = NULL,
    preloader = list(html = NULL, color = "#3c8dbc"),
    navbar = build_navbar(authenticated = TRUE, show_notifications = TRUE, theme_input_id = "theme_toggle"),
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
              shiny::h4(shiny::strong(shiny::textOutput("content_user", container = shiny::span)))
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "setup",
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

  adapt_page_args <- function(args) {
    if (!"navbar" %in% available_args && "header" %in% available_args && "navbar" %in% names(args)) {
      args$header <- args$navbar
      args$navbar <- NULL
    }
    if (!"controlbar" %in% available_args && "rightsidebar" %in% available_args && "controlbar" %in% names(args)) {
      args$rightsidebar <- args$controlbar
      args$controlbar <- NULL
    }
    args[names(args) %in% available_args]
  }

  page_args <- adapt_page_args(page_args)
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

  public_sidebar <- build_sidebar(
    id = "public_nav",
    authenticated = FALSE,
    class = "public-menu public-sidebar"
  )

  public_navbar <- build_navbar(authenticated = FALSE, show_notifications = FALSE, theme_input_id = "theme_toggle_public")

  public_body <- bs4Dash::bs4DashBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "content",
        bs4Dash::bs4Card(
          title = "Uvítací stránka",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          shiny::div(
            class = "text-center py-4",
            shiny::h3("Vítejte v RBudgeting"),
            shiny::p("Přihlaste se pro zobrazení obsahu aplikace."),
            shiny::actionButton(
              "login_content",
              label = "Login",
              icon = shiny::icon("sign-in-alt"),
              class = "btn-primary"
            )
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "setup",
        bs4Dash::bs4Card(
          title = "Instalace systému",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          setup_header,
          shiny::div(
            class = "public-setup-body",
            mod_setup_ui("setup")
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "users",
        bs4Dash::bs4Card(
          title = "Administrace uživatelů",
          status = "secondary",
          solidHeader = TRUE,
          width = 12,
          shiny::div(
            class = "text-center py-4",
            shiny::p("Tato sekce je dostupná pouze po přihlášení."),
            shiny::actionButton(
              "login_users",
              label = "Login",
              icon = shiny::icon("sign-in-alt"),
              class = "btn-primary"
            )
          )
        )
      )
    )
  )

  public_footer <- bs4Dash::bs4DashFooter(
    left = "RBudgeting",
    right = shiny::HTML("<b>Version</b> 0.0.1")
  )

  public_page_args <- adapt_page_args(list(
    title = "RBudgeting",
    freshTheme = NULL,
    preloader = list(html = NULL, color = "#3c8dbc"),
    navbar = public_navbar,
    sidebar = public_sidebar,
    footer = public_footer,
    body = public_body
  ))

  setup_public <- shiny::div(
    id = "public-shell",
    class = "setup-public-container",
    do.call(bs4Dash::bs4DashPage, public_page_args)
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
          "#public-shell.public-hidden { display: none !important; }\n",
          "#public-shell .menu-item-disabled > a { pointer-events: none; opacity: 0.5; }\n",
          "#public-shell .menu-item-disabled .nav-icon { opacity: 0.6; }\n",
          "#public-shell .menu-item-disabled .nav-link { cursor: not-allowed; }"
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
