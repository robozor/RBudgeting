#' Application server logic
#'
#' @import shiny
app_server <- function(input, output, session) {
  db_cfg <- get_db_config()
  app_settings <- get_app_settings()

  conn <- shiny::reactiveVal(NULL)
  default_theme <- app_settings$default_theme %||% "light"
  user_theme <- shiny::reactiveVal(default_theme)

  send_theme <- function(mode) {
    session$sendCustomMessage("toggle-theme", list(mode = mode))
  }

  shiny::observeEvent(TRUE, {
    tryCatch({
      connection <- db_connect(db_cfg)
      db_install_schema(connection)
      conn(connection)
    }, error = function(e) {
      shiny::showNotification(
        paste("Failed to connect to database:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  }, once = TRUE)

  shiny::onStop(function() {
    connection <- shiny::isolate(conn())
    db_disconnect(connection)
  })

  auth <- shinymanager_auth_server(
    "auth",
    session = session,
    check_credentials = credential_checker(conn)
  )

  shinymanager_logout_module(id = "logout", active = shiny::reactive(auth$result))

  is_authenticated <- shiny::reactive({
    isTRUE(auth$result)
  })

  shiny::observeEvent(TRUE, {
    send_theme(user_theme())
  }, once = TRUE)

  current_user <- shiny::reactive({
    shiny::req(is_authenticated())
    info <- auth$info
    fullname <- if (!is.null(info) && !is.null(info$fullname)) info$fullname else ""
    username <- auth$user %||% ""
    preferred <- if (nzchar(fullname)) fullname else username
    if (!nzchar(preferred)) {
      "Neznámý uživatel"
    } else {
      preferred
    }
  })

  output$user_badge <- shiny::renderUI({
    if (is_authenticated()) {
      bslib_badge(current_user(), color = "primary")
    } else {
      bslib_badge("Nepřihlášen", color = "secondary")
    }
  })

  output$auth_control <- shiny::renderUI({
    if (is_authenticated()) {
      shinymanager_logout_ui(
        "logout",
        label = "Odhlásit se",
        class = "btn btn-outline-danger"
      )
    } else {
      shiny::actionButton("show_login", "Přihlásit se", class = "btn btn-primary")
    }
  })

  output$theme_toggle_ui <- shiny::renderUI({
    if (!is_authenticated()) {
      return(NULL)
    }
    shiny::div(
      class = "navbar-text custom-theme-toggle",
      shiny::checkboxInput("theme_dark", "Tmavé téma", value = identical(user_theme(), "dark"))
    )
  })

  shiny::observeEvent(input$show_login, {
    bslib::nav_select(session = session, id = "main_nav", selected = "content")
  })

  shiny::observeEvent(input$show_login_users, {
    bslib::nav_select(session = session, id = "main_nav", selected = "content")
  })

  output$content_nav <- shiny::renderUI({
    if (is_authenticated()) {
      mod_content_ui("content")
    } else {
      bslib::card(
        bslib::card_header("Přihlášení"),
        bslib::card_body(
          shinymanager::auth_ui(
            id = "auth",
            title = "Přihlášení do RBudgeting",
            status = "primary",
            choose_language = FALSE,
            tags_top = shiny::tags$p(
              class = "text-muted",
              sprintf("Aktuální jazyk rozhraní: %s", app_settings$language)
            )
          )
        )
      )
    }
  })

  output$users_panel <- shiny::renderUI({
    if (is_authenticated()) {
      mod_user_management_ui("user_management")
    } else {
      bslib::card(
        bslib::card_header("Administrace uživatelů"),
        bslib::card_body(
          shiny::p("Přihlaste se pro správu uživatelů."),
          shiny::actionButton("show_login_users", "Přejít na přihlášení", class = "btn btn-link")
        )
      )
    }
  })

  shiny::observeEvent(
    list(is_authenticated(), conn()),
    {
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        return()
      }
      if (is_authenticated()) {
        username <- auth$user %||% ""
        if (!nzchar(username)) {
          return()
        }
        preference <- tryCatch(
          db_get_user_theme(connection, username, default = default_theme),
          error = function(e) {
            shinyFeedback::showToast("error", paste("Načtení tématu selhalo:", conditionMessage(e)))
            default_theme
          }
        )
        user_theme(preference)
      } else {
        user_theme(default_theme)
      }
    },
    ignoreNULL = FALSE
  )

  shiny::observeEvent(user_theme(), {
    send_theme(user_theme())
    shiny::updateCheckboxInput(session, "theme_dark", value = identical(user_theme(), "dark"))
  })

  shiny::observeEvent(input$theme_dark, {
    if (!is_authenticated()) {
      shiny::updateCheckboxInput(session, "theme_dark", value = identical(user_theme(), "dark"))
      return()
    }
    value <- input$theme_dark
    if (is.null(value)) {
      return()
    }
    target <- if (isTRUE(value)) "dark" else "light"
    if (identical(target, user_theme())) {
      return()
    }
    connection <- conn()
    if (is.null(connection) || !DBI::dbIsValid(connection)) {
      shinyFeedback::showToast("error", "Databáze není dostupná")
      shiny::updateCheckboxInput(session, "theme_dark", value = identical(user_theme(), "dark"))
      return()
    }
    username <- auth$user %||% ""
    if (!nzchar(username)) {
      shiny::updateCheckboxInput(session, "theme_dark", value = identical(user_theme(), "dark"))
      return()
    }
    tryCatch({
      db_set_user_theme(connection, username, target)
      user_theme(target)
      shinyFeedback::showToast(
        "success",
        if (target == "dark") "Téma aplikace změněno na tmavé." else "Téma aplikace změněno na světlé."
      )
    }, error = function(e) {
      shinyFeedback::showToast("error", paste("Uložení tématu selhalo:", conditionMessage(e)))
      shiny::updateCheckboxInput(session, "theme_dark", value = identical(user_theme(), "dark"))
    })
  }, ignoreNULL = FALSE)

  mod_setup_server("setup", conn = conn, config = db_cfg)
  mod_content_server("content", current_user = current_user, is_authenticated = is_authenticated)

  mod_user_management_server("user_management", conn = conn)
}
