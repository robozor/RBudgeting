#' setup Module UI
#'
#' @param id Internal parameters for {shiny}.
#' @return A UI definition.
mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  cfg <- get_db_config()
  log_structure("mod_setup_ui.config_defaults", cfg)
  shiny::tagList(
    bs4Dash::bs4Card(
      title = "Database installation",
      width = 12,
      status = "primary",
      collapsible = TRUE,
      solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(6,
          shiny::textInput(ns("host"), "Host", value = cfg$host),
          shiny::numericInput(ns("port"), "Port", value = cfg$port, min = 1, max = 65535),
          shiny::textInput(ns("dbname"), "Database", value = cfg$dbname)
        ),
        shiny::column(6,
          shiny::textInput(ns("user"), "User", value = cfg$user),
          shiny::passwordInput(ns("password"), "Password", value = cfg$password),
          shiny::selectInput(ns("sslmode"), "SSL mode", choices = c("disable", "allow", "prefer", "require", "verify-ca", "verify-full"), selected = cfg$sslmode %||% "prefer")
        )
      ),
      shiny::fluidRow(
        shiny::column(3, shiny::actionButton(ns("test"), "Test connection", icon = shiny::icon("plug"))),
        shiny::column(3, shiny::actionButton(ns("install"), "Install schema", icon = shiny::icon("database"))),
        shiny::column(3, shiny::actionButton(ns("store"), "Store configuration", icon = shiny::icon("save"))),
        shiny::column(3, shiny::actionButton(ns("load"), "Load configuration", icon = shiny::icon("folder-open")))
      ),
      shiny::div(
        class = "mt-3",
        shiny::actionButton(ns("show_login"), "Go to login", icon = shiny::icon("sign-in-alt"), class = "btn btn-success")
      )
    ),
    bs4Dash::bs4Card(
      title = "Create initial administrator",
      width = 12,
      status = "info",
      collapsible = TRUE,
      solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(4, shiny::textInput(ns("admin_user"), "Admin username", value = "admin")),
        shiny::column(4, shiny::textInput(ns("admin_name"), "Admin full name")),
        shiny::column(4, shiny::passwordInput(ns("admin_password"), "Admin password"))
      ),
      shiny::actionButton(ns("create_admin"), "Create administrator", icon = shiny::icon("user-shield"))
    )
  )
}

#' setup Module Server
#'
#' @param id module id
#' @param conn Reactive expression containing shared DBI connection
#' @param config Default configuration list
#' @return The updated configuration reactive values
mod_setup_server <- function(id, conn, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_cfg <- shiny::reactiveValues(
      host = config$host,
      port = config$port,
      dbname = config$dbname,
      user = config$user,
      password = config$password,
      sslmode = config$sslmode %||% "prefer"
    )

    gather_cfg <- function() {
      list(
        host = input$host,
        port = input$port,
        dbname = input$dbname,
        user = input$user,
        password = input$password,
        sslmode = input$sslmode
      )
    }

    update_current_cfg <- function(cfg, update_inputs = FALSE) {
      current_cfg$host <- cfg$host
      current_cfg$port <- cfg$port
      current_cfg$dbname <- cfg$dbname
      current_cfg$user <- cfg$user
      current_cfg$password <- cfg$password
      current_cfg$sslmode <- cfg$sslmode

      if (isTRUE(update_inputs)) {
        shiny::updateTextInput(session, "host", value = cfg$host)
        shiny::updateNumericInput(session, "port", value = cfg$port)
        shiny::updateTextInput(session, "dbname", value = cfg$dbname)
        shiny::updateTextInput(session, "user", value = cfg$user)
        shiny::updateTextInput(session, "password", value = cfg$password)
        shiny::updateSelectInput(session, "sslmode", selected = cfg$sslmode)
      }
    }

    reconnect <- function(cfg) {
      existing <- conn()
      if (!is.null(existing) && DBI::dbIsValid(existing)) {
        db_disconnect(existing)
      }
      conn(db_connect(cfg))
      shinyFeedback::showToast("success", "Database connection refreshed")
      add_notification(session, "Database connection refreshed", status = "success", icon = "plug")
    }

    observe_result <- function(expr, success_msg, error_msg, status = "success", icon = "check") {
      tryCatch({
        expr
        shinyFeedback::showToast("success", success_msg)
        add_notification(session, success_msg, status = status, icon = icon)
      }, error = function(e) {
        message <- conditionMessage(e)
        shinyFeedback::showToast("error", paste0(error_msg, ": ", message))
        add_notification(session, paste0(error_msg, ": ", message), status = "danger", icon = "exclamation-triangle")
      })
    }

    shiny::observeEvent(input$test, {
      cfg <- gather_cfg()
      observe_result({
        temp_conn <- db_connect(cfg)
        on.exit(db_disconnect(temp_conn))
        DBI::dbGetQuery(temp_conn, "SELECT 1;")
      }, success_msg = "Connection succeeded", error_msg = "Connection failed", status = "success", icon = "plug")
    })

    shiny::observeEvent(input$install, {
      cfg <- gather_cfg()
      observe_result({
        temp_conn <- db_connect(cfg)
        on.exit(db_disconnect(temp_conn))
        db_install_schema(temp_conn)
      }, success_msg = "Schema ready", error_msg = "Schema installation failed", status = "primary", icon = "database")
    })

    shiny::observeEvent(input$create_admin, {
      cfg <- gather_cfg()
      shiny::req(input$admin_user, input$admin_password)
      observe_result({
        temp_conn <- db_connect(cfg)
        on.exit(db_disconnect(temp_conn))
        db_ensure_admin(temp_conn, input$admin_user, input$admin_password, input$admin_name)
      }, success_msg = "Administrator ready", error_msg = "Admin creation failed", status = "info", icon = "user-shield")
    })

    shiny::observeEvent(input$store, {
      cfg <- gather_cfg()
      observe_result({
        save_db_config(cfg)
        sanitized <- sanitize_db_configuration(cfg)
        update_current_cfg(sanitized)
        reconnect(sanitized)
      }, success_msg = "Configuration stored", error_msg = "Failed to store configuration", status = "info", icon = "save")
    })

    shiny::observeEvent(input$load, {
      observe_result({
        loaded <- load_persisted_db_config()
        if (is.null(loaded)) {
          stop("No configuration file available")
        }
        sanitized <- sanitize_db_configuration(loaded)
        update_current_cfg(sanitized, update_inputs = TRUE)
        reconnect(sanitized)
      }, success_msg = "Configuration loaded", error_msg = "Unable to load configuration", status = "info", icon = "folder-open")
    })

    shiny::observeEvent(input$show_login, {
      shinyjs::removeClass(selector = "#secure-content", class = "secure-hidden")
    })

    shiny::reactive(current_cfg)
  })
}
