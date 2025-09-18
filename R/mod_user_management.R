#' user_management Module UI
#'
#' @param id Module id
mod_user_management_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("Seznam uživatelů"),
      bslib::card_body(DT::DTOutput(ns("table")))
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header("Přidat nebo upravit uživatele"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::textInput(ns("username"), "Uživatelské jméno"),
            shiny::textInput(ns("fullname"), "Celé jméno")
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(ns("role"), "Role", choices = c("user", "manager", "admin")),
            shiny::checkboxInput(ns("is_active"), "Aktivní", value = TRUE)
          ),
          shiny::passwordInput(ns("password"), "Heslo"),
          shiny::actionButton(ns("save"), "Uložit uživatele", icon = shiny::icon("save"), class = "btn btn-primary")
        )
      ),
      bslib::card(
        bslib::card_header("Další akce"),
        bslib::card_body(
          shiny::selectInput(ns("selected_user"), "Vyberte uživatele", choices = NULL),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::actionButton(ns("deactivate"), "Přepnout aktivitu", icon = shiny::icon("user-slash"), class = "btn btn-outline-primary"),
            shiny::actionButton(ns("delete"), "Smazat", icon = shiny::icon("trash"), class = "btn btn-outline-danger")
          )
        )
      )
    )
  )
}

#' user_management Module Server
#'
#' @param id Module id
#' @param conn Reactive value containing DB connection
#' @param auth Deprecated
mod_user_management_server <- function(id, conn, auth = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    user_admin_log <- function(event, ...) {
      fragments <- c(event, ...)
      if (length(fragments) == 0) {
        return(invisible(NULL))
      }
      formatted <- vapply(
        fragments,
        function(value) format_scalar_for_log(value, fallback = "<empty>"),
        character(1)
      )
      message(sprintf("[user_admin] %s", paste(formatted, collapse = " | ")))
      invisible(NULL)
    }

    users <- shiny::reactiveVal(NULL)

    empty_user_table <- dplyr::tibble(
      id = integer(),
      username = character(),
      fullname = character(),
      role = character(),
      is_active = logical(),
      created_at = as.POSIXct(character()),
      updated_at = as.POSIXct(character())
    )

    update_user_choices <- function(data) {
      choices <- character()
      if (!is.null(data) && "username" %in% names(data)) {
        choices <- sort(unique(as.character(data$username)))
      }
      shiny::updateSelectInput(session, "selected_user", choices = choices)
    }

    load_users <- function(connection = NULL) {
      if (is.null(connection)) {
        connection <- conn()
      }

      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        user_admin_log("load_users:skipped", "connection_unavailable")
        users(NULL)
        update_user_choices(NULL)
        return(invisible(FALSE))
      }

      user_admin_log("load_users:start")

      tryCatch({
        data <- db_get_users(connection)
        row_count <- nrow(data)
        user_admin_log("load_users:success", sprintf("rows=%s", row_count))
        users(data)
        update_user_choices(data)
        invisible(TRUE)
      }, error = function(e) {
        user_admin_log("load_users:error", conditionMessage(e))
        users(NULL)
        update_user_choices(NULL)
        shinyFeedback::showToast("error", "Načtení uživatelů se nezdařilo.")
        invisible(FALSE)
      })
    }

    initial_connection <- shiny::isolate(conn())
    if (!is.null(initial_connection) && DBI::dbIsValid(initial_connection)) {
      user_admin_log("init:connection_ready")
      load_users(initial_connection)
    } else {
      user_admin_log("init:connection_pending")
    }

    output$table <- DT::renderDT({
      data <- users()
      if (is.null(data)) {
        log_debug("user_management:render", "User data not yet available; showing empty table.")
        return(empty_user_table)
      }

      row_count <- nrow(data)
      log_debug("user_management:render", sprintf("Rendering table with %s rows.", row_count))
      log_structure("user_management:render", data)

      if (row_count == 0) {
        log_debug("user_management:render", "User dataset is empty; preserving column structure.")
      }

      data
    }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)

    shiny::observeEvent(input$save, {
      shiny::req(input$username, input$password)
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Databáze není dostupná")
        return()
      }
      username <- input$username
      user_admin_log("user_save:attempt", username)
      tryCatch({
        db_upsert_user(connection, username, input$password, fullname = input$fullname, role = input$role, active = input$is_active)
        shinyFeedback::showToast("success", "Uživatel uložen")
        user_admin_log("user_save:success", username)
        load_users()
      }, error = function(e) {
        shinyFeedback::showToast("error", paste("Uložení se nezdařilo:", conditionMessage(e)))
        user_admin_log("user_save:error", username, conditionMessage(e))
      })
    })

    shiny::observeEvent(input$deactivate, {
      shiny::req(input$selected_user)
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Databáze není dostupná")
        return()
      }
      selected <- users()
      if (is.null(selected)) {
        user_admin_log("user_toggle:skipped", "users_not_loaded")
        return()
      }
      selected <- dplyr::filter(selected, username == input$selected_user)
      if (nrow(selected) == 0) {
        user_admin_log("user_toggle:skipped", sprintf("user=%s_not_found", input$selected_user))
        return()
      }
      new_state <- !isTRUE(selected$is_active[1])
      user_admin_log("user_toggle:attempt", input$selected_user, sprintf("target_state=%s", new_state))
      tryCatch({
        db_set_user_active(connection, input$selected_user, new_state)
        shinyFeedback::showToast("info", paste0("Uživatel ", input$selected_user, ifelse(new_state, " aktivován", " deaktivován")))
        user_admin_log("user_toggle:success", input$selected_user, sprintf("target_state=%s", new_state))
        load_users()
      }, error = function(e) {
        shinyFeedback::showToast("error", paste("Změna stavu selhala:", conditionMessage(e)))
        user_admin_log("user_toggle:error", input$selected_user, conditionMessage(e))
      })
    })

    shiny::observeEvent(input$delete, {
      shiny::req(input$selected_user)
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Databáze není dostupná")
        return()
      }
      user_admin_log("user_delete:attempt", input$selected_user)
      tryCatch({
        db_delete_user(connection, input$selected_user)
        shinyFeedback::showToast("warning", paste0("Uživatel ", input$selected_user, " odstraněn"))
        user_admin_log("user_delete:success", input$selected_user)
        load_users()
      }, error = function(e) {
        shinyFeedback::showToast("error", paste("Smazání selhalo:", conditionMessage(e)))
        user_admin_log("user_delete:error", input$selected_user, conditionMessage(e))
      })
    })

    shiny::observeEvent(conn(), {
      load_users()
    }, ignoreNULL = FALSE)
  })
}
