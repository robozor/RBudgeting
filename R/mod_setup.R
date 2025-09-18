#' setup Module UI
#'
#' @param id Internal parameters for {shiny}.
#' @return A UI definition.
mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  cfg <- get_db_config()
  log_structure("mod_setup_ui.config_defaults", redact_sensitive(cfg))

  bslib::card(
    bslib::card_header("Konfigurace databáze"),
    bslib::card_body(
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::div(
          class = "d-grid gap-3",
          shiny::textInput(ns("host"), "Host", value = cfg$host),
          shiny::numericInput(ns("port"), "Port", value = cfg$port, min = 1, max = 65535),
          shiny::textInput(ns("dbname"), "Databáze", value = cfg$dbname)
        ),
        shiny::div(
          class = "d-grid gap-3",
          shiny::textInput(ns("user"), "Uživatel", value = cfg$user),
          shiny::passwordInput(ns("password"), "Heslo", value = cfg$password),
          shiny::selectInput(
            ns("sslmode"),
            "Režim SSL",
            choices = c("disable", "allow", "prefer", "require", "verify-ca", "verify-full"),
            selected = cfg$sslmode %||% "prefer"
          )
        )
      ),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        shiny::actionButton(ns("test"), "Test připojení", icon = shiny::icon("plug"), class = "btn btn-outline-primary w-100"),
        shiny::actionButton(ns("install"), "Instalovat schéma", icon = shiny::icon("database"), class = "btn btn-outline-primary w-100"),
        shiny::actionButton(ns("store"), "Uložit konfiguraci", icon = shiny::icon("save"), class = "btn btn-outline-primary w-100"),
        shiny::actionButton(ns("load"), "Načíst konfiguraci", icon = shiny::icon("folder-open"), class = "btn btn-outline-primary w-100")
      ),
      shiny::tags$hr(class = "my-4"),
      shiny::div(
        class = "d-grid gap-3",
        shiny::tags$h4("Vytvoření administrátora"),
        shiny::textInput(ns("admin_user"), "Uživatelské jméno", value = "admin"),
        shiny::textInput(ns("admin_name"), "Celé jméno"),
        shiny::passwordInput(ns("admin_password"), "Heslo"),
        shiny::actionButton(ns("create_admin"), "Vytvořit administrátora", icon = shiny::icon("user-shield"), class = "btn btn-primary w-100")
      )
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
      shinyFeedback::showToast("success", "Spojení s databází obnoveno")
    }

    observe_result <- function(expr, success_msg, error_msg) {
      tryCatch({
        expr
        shinyFeedback::showToast("success", success_msg)
      }, error = function(e) {
        message <- conditionMessage(e)
        shinyFeedback::showToast("error", paste0(error_msg, ": ", message))
      })
    }

    shiny::observeEvent(input$test, {
      cfg <- gather_cfg()
      observe_result({
        temp_conn <- db_connect(cfg)
        on.exit(db_disconnect(temp_conn))
        DBI::dbGetQuery(temp_conn, "SELECT 1;")
      }, success_msg = "Připojení úspěšné", error_msg = "Test připojení selhal")
    })

    shiny::observeEvent(input$install, {
      cfg <- gather_cfg()
      observe_result({
        temp_conn <- db_connect(cfg)
        on.exit(db_disconnect(temp_conn))
        db_install_schema(temp_conn)
      }, success_msg = "Schéma připraveno", error_msg = "Instalace schématu selhala")
    })

    shiny::observeEvent(input$create_admin, {
      cfg <- gather_cfg()
      shiny::req(input$admin_user, input$admin_password)
      observe_result({
        temp_conn <- db_connect(cfg)
        on.exit(db_disconnect(temp_conn))
        db_ensure_admin(temp_conn, input$admin_user, input$admin_password, input$admin_name)
      }, success_msg = "Administrátor vytvořen", error_msg = "Vytvoření administrátora selhalo")
    })

    shiny::observeEvent(input$store, {
      cfg <- gather_cfg()
      observe_result({
        save_db_config(cfg)
        sanitized <- sanitize_db_configuration(cfg)
        update_current_cfg(sanitized)
        reconnect(sanitized)
      }, success_msg = "Konfigurace uložena", error_msg = "Konfiguraci se nepodařilo uložit")
    })

    shiny::observeEvent(input$load, {
      observe_result({
        loaded <- load_persisted_db_config()
        if (is.null(loaded)) {
          stop("Konfigurační soubor není k dispozici")
        }
        sanitized <- sanitize_db_configuration(loaded)
        update_current_cfg(sanitized, update_inputs = TRUE)
        reconnect(sanitized)
      }, success_msg = "Konfigurace načtena", error_msg = "Konfiguraci se nepodařilo načíst")
    })

    shiny::reactive(current_cfg)
  })
}
