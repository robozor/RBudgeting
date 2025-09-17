#' Application server logic
#'
#' @import shiny
app_server <- function(input, output, session) {
  db_cfg <- get_db_config()
  conn <- shiny::reactiveVal(NULL)

  shiny::observeEvent(TRUE, {
    tryCatch({
      connection <- db_connect(db_cfg)
      db_install_schema(connection)
      conn(connection)
    }, error = function(e) {
      shiny::showNotification(
        paste("Failed to connect to database:", e$message),
        type = "error",
        duration = NULL
      )
    })
  }, once = TRUE)

  shiny::onStop(function() {
    db_disconnect(conn())
  })

  auth <- shinymanager::secure_server(
    check_credentials = credential_checker(conn)
  )

  notifications <- shiny::reactiveVal(list(
    list(text = "Welcome to RBudgeting", status = "info", icon = "info-circle"),
    list(text = "Use the Setup tab to configure the database", status = "primary", icon = "tools")
  ))

  output$notifications <- bs4Dash::renderMenu({
    items <- notifications()
    args <- list(
      type = "notifications",
      badgeStatus = if (length(items) > 0) items[[1]]$status else NULL,
      headerText = "Notifications"
    )
    if (length(items) > 0) {
      args <- c(
        args,
        lapply(items, function(item) {
          bs4Dash::notificationItem(
            text = item$text,
            status = item$status,
            icon = shiny::icon(item$icon)
          )
        })
      )
    }
    do.call(bs4Dash::dropdownMenu, args)
  })

  shiny::observeEvent(input$logout, {
    shinymanager::logout(session = session)
  })

  shiny::observeEvent(conn(), {
    connection <- conn()
    if (is.null(connection) || !DBI::dbIsValid(connection)) {
      shiny::showNotification("Database connection not available", type = "error")
    }
  })

  shiny::observeEvent(input$theme_toggle, {
    mode <- if (isTRUE(input$theme_toggle)) "dark" else "light"
    session$sendCustomMessage("toggle-theme", list(mode = mode))
  })

  shiny::observeEvent(TRUE, {
    default <- get_golem_config()$app$default_theme %||% "light"
    session$sendCustomMessage("toggle-theme", list(mode = default))
    shinyWidgets::updateMaterialSwitch(session, "theme_toggle", value = identical(default, "dark"))
  }, once = TRUE)

  mod_setup_server("setup", conn = conn, config = db_cfg)
  mod_user_management_server("user_management", conn = conn)

  session$userData$notifications <- notifications
}
