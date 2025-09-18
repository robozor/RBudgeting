#' Application server logic
#'
#' @import shiny
app_server <- function(input, output, session) {
  db_cfg <- get_db_config()
  app_settings <- get_app_settings()
  message(
    sprintf(
      "[app_server] Database config: host=%s, port=%s, dbname=%s, user=%s, sslmode=%s",
      format_scalar_for_log(db_cfg$host),
      format_scalar_for_log(db_cfg$port),
      format_scalar_for_log(db_cfg$dbname),
      format_scalar_for_log(db_cfg$user),
      format_scalar_for_log(db_cfg$sslmode)
    )
  )
  message(
    sprintf(
      "[app_server] App settings: language=%s", format_scalar_for_log(app_settings$language)
    )
  )

  conn <- shiny::reactiveVal(NULL)

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

  auth <- shinymanager::auth_server(
    check_credentials = credential_checker(conn),
    session = session
  )

  shinymanager_logout_module(id = "logout", active = shiny::reactive(auth$result))

  is_authenticated <- shiny::reactive({
    isTRUE(auth$result)
  })

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
      bslib::badge(current_user(), color = "primary")
    } else {
      bslib::badge("Nepřihlášen", color = "secondary")
    }
  })

  output$auth_control <- shiny::renderUI({
    if (is_authenticated()) {
      shinymanager::logoutUI("logout", label = "Odhlásit se", class = "btn btn-outline-danger")
    } else {
      shiny::actionButton("show_login", "Přihlásit se", class = "btn btn-primary")
    }
  })

  shiny::observeEvent(input$show_login, {
    bslib::update_navs(session, "main_nav", selected = "content")
  })

  shiny::observeEvent(input$show_login_users, {
    bslib::update_navs(session, "main_nav", selected = "content")
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

  mod_setup_server("setup", conn = conn, config = db_cfg)
  mod_content_server("content", current_user = current_user, is_authenticated = is_authenticated)

  shiny::observeEvent(is_authenticated(), {
    shiny::req(is_authenticated())
    mod_user_management_server("user_management", conn = conn)
  }, once = TRUE)
}
