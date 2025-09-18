#' user_management Module UI
#'
#' @param id Module id
mod_user_management_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("Seznam uživatelů"),
      bslib::card_body(DT::dataTableOutput(ns("table")))
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
    
    users <- shiny::reactiveVal(dplyr::tibble())

    load_users <- function(connection = NULL) {
      if (is.null(connection)) {
        connection <- conn()
      }

      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        return()
      }

      try({
        data <- db_get_users(connection)
        users(data)
        shiny::updateSelectInput(session, "selected_user", choices = data$username)
      }, silent = TRUE)
    }

    initial_connection <- shiny::isolate(conn())
    if (!is.null(initial_connection) && DBI::dbIsValid(initial_connection)) {
      load_users(initial_connection)
    }

    output$table <- DT::renderDT({
      users()
    }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)

    shiny::observeEvent(input$save, {
      shiny::req(input$username, input$password)
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Databáze není dostupná")
        return()
      }
      tryCatch({
        db_upsert_user(connection, input$username, input$password, fullname = input$fullname, role = input$role, active = input$is_active)
        shinyFeedback::showToast("success", "Uživatel uložen")
        load_users()
      }, error = function(e) {
        message <- conditionMessage(e)
        shinyFeedback::showToast("error", paste("Uložení se nezdařilo:", message))
      })
    })

    shiny::observeEvent(input$deactivate, {
      shiny::req(input$selected_user)
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Databáze není dostupná")
        return()
      }
      selected <- dplyr::filter(users(), username == input$selected_user)
      if (nrow(selected) == 0) {
        return()
      }
      new_state <- !isTRUE(selected$is_active[1])
      tryCatch({
        db_set_user_active(connection, input$selected_user, new_state)
        shinyFeedback::showToast("info", paste0("Uživatel ", input$selected_user, ifelse(new_state, " aktivován", " deaktivován")))
        load_users()
      }, error = function(e) {
        message <- conditionMessage(e)
        shinyFeedback::showToast("error", paste("Změna stavu selhala:", message))
      })
    })

    shiny::observeEvent(input$delete, {
      shiny::req(input$selected_user)
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Databáze není dostupná")
        return()
      }
      tryCatch({
        db_delete_user(connection, input$selected_user)
        shinyFeedback::showToast("warning", paste0("Uživatel ", input$selected_user, " odstraněn"))
        load_users()
      }, error = function(e) {
        message <- conditionMessage(e)
        shinyFeedback::showToast("error", paste("Smazání selhalo:", message))
      })
    })

    shiny::observeEvent(conn(), {
      load_users()
    })
  })
}
