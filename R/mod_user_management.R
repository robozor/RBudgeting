#' user_management Module UI
#'
#' @param id Module id
mod_user_management_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bs4Dash::bs4Card(
      title = "Users",
      width = 12,
      status = "primary",
      collapsible = TRUE,
      solidHeader = TRUE,
      DT::dataTableOutput(ns("table"))
    ),
    bs4Dash::bs4Card(
      title = "Add or update user",
      width = 12,
      status = "info",
      collapsible = TRUE,
      solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(4, shiny::textInput(ns("username"), "Username")),
        shiny::column(4, shiny::textInput(ns("fullname"), "Full name")),
        shiny::column(4, shiny::selectInput(ns("role"), "Role", choices = c("user", "manager", "admin")))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::passwordInput(ns("password"), "Password")),
        shiny::column(6, shiny::checkboxInput(ns("is_active"), "Active", value = TRUE))
      ),
      shiny::actionButton(ns("save"), "Save user", icon = shiny::icon("save"))
    ),
    bs4Dash::bs4Card(
      title = "Deactivate or delete",
      width = 12,
      status = "warning",
      collapsible = TRUE,
      solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("selected_user"), "Select user", choices = NULL)),
        shiny::column(6, shiny::actionButton(ns("deactivate"), "Toggle active", icon = shiny::icon("user-slash")))
      ),
      shiny::actionButton(ns("delete"), "Delete user", icon = shiny::icon("trash"), class = "btn-danger")
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
    ns <- session$ns

    users <- shiny::reactiveVal(dplyr::tibble())

    load_users <- function() {
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        return()
      }
      try({
        users(db_get_users(connection))
        shiny::updateSelectInput(session, "selected_user", choices = users()$username)
      }, silent = TRUE)
    }

    output$table <- DT::renderDT({
      users()
    }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)

    shiny::observeEvent(input$save, {
      shiny::req(input$username, input$password)
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Database unavailable")
        return()
      }
      tryCatch({
        db_upsert_user(connection, input$username, input$password, fullname = input$fullname, role = input$role, active = input$is_active)
        shinyFeedback::showToast("success", "User saved")
        add_notification(session, paste0("User ", input$username, " saved"), status = "success", icon = "user")
        load_users()
      }, error = function(e) {
        shinyFeedback::showToast("error", paste("Unable to save user:", e$message))
        add_notification(session, paste("Unable to save user:", e$message), status = "danger", icon = "exclamation-triangle")
      })
    })

    shiny::observeEvent(input$deactivate, {
      shiny::req(input$selected_user)
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Database unavailable")
        return()
      }
      selected <- users() %>% dplyr::filter(username == input$selected_user)
      if (nrow(selected) == 0) {
        return()
      }
      new_state <- !isTRUE(selected$is_active[1])
      tryCatch({
        db_set_user_active(connection, input$selected_user, new_state)
        shinyFeedback::showToast("info", paste0("User ", input$selected_user, ifelse(new_state, " activated", " deactivated")))
        add_notification(session, paste0("User ", input$selected_user, ifelse(new_state, " activated", " deactivated")), status = "info", icon = "user-check")
        load_users()
      }, error = function(e) {
        shinyFeedback::showToast("error", paste("Unable to change state:", e$message))
        add_notification(session, paste("Unable to change user state:", e$message), status = "danger", icon = "exclamation-triangle")
      })
    })

    shiny::observeEvent(input$delete, {
      shiny::req(input$selected_user)
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Database unavailable")
        return()
      }
      tryCatch({
        db_delete_user(connection, input$selected_user)
        shinyFeedback::showToast("warning", paste0("User ", input$selected_user, " deleted"))
        add_notification(session, paste0("User ", input$selected_user, " deleted"), status = "warning", icon = "trash")
        load_users()
      }, error = function(e) {
        shinyFeedback::showToast("error", paste("Unable to delete user:", e$message))
        add_notification(session, paste("Unable to delete user:", e$message), status = "danger", icon = "exclamation-triangle")
      })
    })

    shiny::observeEvent(conn(), {
      load_users()
    })
  })
}
