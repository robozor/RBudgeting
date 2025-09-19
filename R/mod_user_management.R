user_role_choices <- c("user", "manager", "admin")

#' user_management Module UI
#'
#' @param id Module id
mod_user_management_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("Seznam uživatelů"),
      bslib::card_body(
        shiny::div(
          class = "d-flex flex-wrap gap-2 justify-content-end mb-3",
          shiny::actionButton(
            ns("edit_user"),
            "Editace",
            icon = shiny::icon("pen-to-square"),
            class = "btn btn-outline-secondary"
          ),
          shiny::actionButton(
            ns("set_inactive"),
            "Nastavit na neaktivní",
            icon = shiny::icon("user-slash"),
            class = "btn btn-outline-secondary"
          ),
          shiny::actionButton(
            ns("set_active"),
            "Nastavit na aktivní",
            icon = shiny::icon("user-check"),
            class = "btn btn-outline-primary"
          ),
          shiny::actionButton(
            ns("delete_selected"),
            "Smazat uživatele",
            icon = shiny::icon("trash"),
            class = "btn btn-outline-danger"
          )
        ),
        DT::DTOutput(ns("table"))
      )
    ),
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
          shiny::selectInput(ns("role"), "Role", choices = user_role_choices),
          shiny::checkboxInput(ns("is_active"), "Aktivní", value = TRUE)
        ),
        shiny::passwordInput(ns("password"), "Heslo"),
        shiny::actionButton(ns("save"), "Uložit uživatele", icon = shiny::icon("save"), class = "btn btn-primary")
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
    ns <- session$ns

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
    edit_state <- shiny::reactiveValues(username = NULL)

    empty_user_table <- dplyr::tibble(
      id = integer(),
      username = character(),
      fullname = character(),
      role = character(),
      is_active = logical(),
      theme_preference = character(),
      created_at = as.POSIXct(character()),
      updated_at = as.POSIXct(character())
    )

    get_selected_usernames <- function() {
      data <- users()
      selected_rows <- input$table_rows_selected
      if (is.null(data) || nrow(data) == 0 || length(selected_rows) == 0) {
        return(character())
      }
      valid_rows <- selected_rows[selected_rows %in% seq_len(nrow(data))]
      if (length(valid_rows) == 0) {
        return(character())
      }
      unique(as.character(data$username[valid_rows]))
    }

    load_users <- function(connection = NULL) {
      if (is.null(connection)) {
        connection <- conn()
      }

      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        user_admin_log("load_users:skipped", "connection_unavailable")
        users(NULL)
        return(invisible(FALSE))
      }

      user_admin_log("load_users:start")

      tryCatch({
        data <- db_get_users(connection)
        row_count <- nrow(data)
        user_admin_log("load_users:success", sprintf("rows=%s", row_count))
        users(data)
        invisible(TRUE)
      }, error = function(e) {
        user_admin_log("load_users:error", conditionMessage(e))
        users(NULL)
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
    }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "multiple")

    shiny::observeEvent(input$edit_user, {
      data <- users()
      if (is.null(data) || nrow(data) == 0) {
        shinyFeedback::showToast("warning", "Nejsou dostupní žádní uživatelé k úpravě.")
        user_admin_log("user_edit:skipped", "no_users")
        return()
      }

      usernames <- get_selected_usernames()
      if (length(usernames) == 0) {
        shinyFeedback::showToast("warning", "Vyberte uživatele pro editaci.")
        user_admin_log("user_edit:skipped", "no_selection")
        return()
      }

      if (length(usernames) > 1) {
        shinyFeedback::showToast("warning", "Pro editaci vyberte právě jednoho uživatele.")
        user_admin_log("user_edit:skipped", sprintf("multiple_selection=%s", length(usernames)))
        return()
      }

      username <- usernames[1]
      row <- data[data$username == username, , drop = FALSE]

      if (nrow(row) != 1) {
        shinyFeedback::showToast("error", "Vybraného uživatele se nepodařilo načíst.")
        user_admin_log("user_edit:error", username, "row_not_found")
        return()
      }

      edit_state$username <- username

      fullname_value <- row$fullname[1]
      if (is.na(fullname_value)) {
        fullname_value <- ""
      }

      role_value <- row$role[1]
      if (is.na(role_value) || !role_value %in% user_role_choices) {
        role_value <- user_role_choices[1]
      }

      shiny::showModal(
        shiny::modalDialog(
          title = sprintf("Editace uživatele %s", username),
          shiny::textInput(ns("edit_fullname"), "Celé jméno", value = fullname_value),
          shiny::selectInput(ns("edit_role"), "Role", choices = user_role_choices, selected = role_value),
          footer = shiny::tagList(
            shiny::modalButton("Zrušit"),
            shiny::actionButton(ns("edit_save"), "Uložit změny", class = "btn btn-primary")
          ),
          easyClose = FALSE
        )
      )
    })

    shiny::observeEvent(input$edit_save, {
      username <- edit_state$username
      if (is.null(username)) {
        shinyFeedback::showToast("error", "Uložit změny není možné bez vybraného uživatele.")
        user_admin_log("user_edit:skipped", "missing_context")
        return()
      }

      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Databáze není dostupná")
        return()
      }

      fullname <- input$edit_fullname
      if (!is.null(fullname)) {
        fullname <- trimws(fullname)
        if (!nzchar(fullname)) {
          fullname <- NULL
        }
      }

      role <- input$edit_role
      if (is.null(role) || !nzchar(role)) {
        shinyFeedback::showToast("warning", "Vyberte platnou roli.")
        user_admin_log("user_edit:skipped", username, "invalid_role")
        return()
      }

      user_admin_log("user_edit:attempt", username)
      tryCatch({
        db_update_user_profile(connection, username, fullname = fullname, role = role)
        shinyFeedback::showToast("success", "Změny byly uloženy.")
        user_admin_log("user_edit:success", username)
        shiny::removeModal()
        edit_state$username <- NULL
        load_users(connection)
      }, error = function(e) {
        shinyFeedback::showToast("error", paste("Uložení změn selhalo:", conditionMessage(e)))
        user_admin_log("user_edit:error", username, conditionMessage(e))
      })
    })

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

    change_user_state <- function(target_state, log_event) {
      usernames <- get_selected_usernames()
      if (length(usernames) == 0) {
        shinyFeedback::showToast("warning", "Vyberte alespoň jednoho uživatele.")
        user_admin_log(sprintf("%s:skipped", log_event), "no_selection")
        return()
      }
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Databáze není dostupná")
        return()
      }
      user_admin_log(sprintf("%s:attempt", log_event), sprintf("count=%s", length(usernames)), paste(usernames, collapse = ","))
      result <- tryCatch({
        DBI::dbWithTransaction(connection, {
          for (username in usernames) {
            db_set_user_active(connection, username, target_state)
          }
        })
        TRUE
      }, error = function(e) {
        shinyFeedback::showToast("error", paste("Změna stavu selhala:", conditionMessage(e)))
        user_admin_log(sprintf("%s:error", log_event), conditionMessage(e))
        FALSE
      })
      if (isTRUE(result)) {
        state_label <- ifelse(target_state, "aktivní", "neaktivní")
        label <- if (length(usernames) == 1) {
          sprintf("Uživatel %s nastaven jako %s.", usernames, state_label)
        } else {
          sprintf("Uživatelé (%s) nastaveni jako %s.", paste(usernames, collapse = ", "), state_label)
        }
        shinyFeedback::showToast("info", label)
        user_admin_log(sprintf("%s:success", log_event), sprintf("count=%s", length(usernames)))
        load_users()
      }
    }

    shiny::observeEvent(input$set_inactive, {
      change_user_state(FALSE, "user_deactivate")
    })

    shiny::observeEvent(input$set_active, {
      change_user_state(TRUE, "user_activate")
    })

    shiny::observeEvent(input$delete_selected, {
      usernames <- get_selected_usernames()
      if (length(usernames) == 0) {
        shinyFeedback::showToast("warning", "Vyberte alespoň jednoho uživatele.")
        user_admin_log("user_delete:skipped", "no_selection")
        return()
      }
      connection <- conn()
      if (is.null(connection) || !DBI::dbIsValid(connection)) {
        shinyFeedback::showToast("error", "Databáze není dostupná")
        return()
      }
      user_admin_log("user_delete:attempt", sprintf("count=%s", length(usernames)), paste(usernames, collapse = ","))
      result <- tryCatch({
        DBI::dbWithTransaction(connection, {
          for (username in usernames) {
            db_delete_user(connection, username)
          }
        })
        TRUE
      }, error = function(e) {
        shinyFeedback::showToast("error", paste("Smazání selhalo:", conditionMessage(e)))
        user_admin_log("user_delete:error", conditionMessage(e))
        FALSE
      })
      if (isTRUE(result)) {
        label <- if (length(usernames) == 1) {
          sprintf("Uživatel %s odstraněn ze systému.", usernames)
        } else {
          sprintf("Uživatelé (%s) odstraněni ze systému.", paste(usernames, collapse = ", "))
        }
        shinyFeedback::showToast("warning", label)
        user_admin_log("user_delete:success", sprintf("count=%s", length(usernames)))
        load_users()
      }
    })

    shiny::observeEvent(conn(), {
      load_users()
    }, ignoreNULL = FALSE)
  })
}
