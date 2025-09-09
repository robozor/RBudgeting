# R/mod_auth.R — doplněno refresh()

`%||%` <- function(a,b) if (is.null(a) || is.na(a) || identical(a, "")) b else a

# --- UI: Přihlášení (bs4Dash) ---
mod_auth_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(
      width = 6, offset = 3,
      bs4Box(
        title = tagList(icon("sign-in-alt"), span(" Přihlášení")),
        status = "primary", solidHeader = TRUE, width = 12, closable = FALSE,
        textInput(ns("email"), "E-mail"),
        passwordInput(ns("password"), "Heslo"),
        actionButton(ns("login"), label = "Přihlásit se",
                     icon = icon("unlock-alt"), class = "btn btn-primary"),
        uiOutput(ns("msg"))
      )
    )
  )
}


mod_auth_server <- function(id, pool){
  shiny::moduleServer(id, function(input, output, session){
    if (!requireNamespace("sodium", quietly = TRUE)) shiny::validate("Install 'sodium'")
    user_rv <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(input$login, {
      shiny::req(input$email, input$password)
      u <- try(sql_get_user_by_email(pool, tolower(trimws(input$email))), silent = TRUE)
      if (inherits(u, "try-error") || nrow(u)==0 || !isTRUE(u$is_active[1])) {
        output$msg <- shiny::renderUI(shiny::div(class="text-danger","Neplatný e-mail/heslo nebo zablokovaný účet."))
        return()
      }
      ok <- try(sodium::password_verify(u$password_hash[1], input$password), silent = TRUE)
      if (!isTRUE(ok)){
        output$msg <- shiny::renderUI(shiny::div(class="text-danger","Neplatný e-mail nebo heslo."))
        return()
      }
      roles <- sql_get_roles_for_user(pool, u$user_id[1])
      sql_touch_last_login(pool, u$user_id[1])
      sql_session_login(pool, session$token, u$user_id[1],
                        session$request$REMOTE_ADDR %||% NA,
                        session$request$HTTP_USER_AGENT %||% NA)
      user_rv(list(
        user_id = u$user_id[1],
        email = u$email[1],
        display_name = u$display_name[1],
        roles = roles
      ))
    })
    
    # NOVÉ: ruční načtení aktuálního stavu uživatele (role/aktivita) z DB
    refresh <- function(){
      u <- user_rv()
      if (is.null(u)) return(invisible(FALSE))
      row <- try(sql_get_user_by_email(pool, u$email), silent = TRUE)
      if (inherits(row, "try-error") || nrow(row)==0) { user_rv(NULL); return(invisible(FALSE)) }
      if (!isTRUE(row$is_active[1])) { sql_session_logout(pool, session$token); user_rv(NULL); return(invisible(FALSE)) }
      roles <- sql_get_roles_for_user(pool, row$user_id[1])
      user_rv(list(
        user_id = row$user_id[1],
        email = row$email[1],
        display_name = row$display_name[1],
        roles = roles
      ))
      invisible(TRUE)
    }
    
    session$onSessionEnded(function(){ try(sql_session_logout(pool, session$token), silent=TRUE) })
    
    list(
      user    = shiny::reactive(user_rv()),
      logout  = function(){ u <- user_rv(); if(!is.null(u)) sql_session_logout(pool, session$token); user_rv(NULL) },
      refresh = refresh   # ← přidáno
    )
  })
}
