# R/mod_admin_users.R — správa uživatelů (box + shiny::modalDialog modály)

mod_admin_users_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = tagList(icon("users-cog"), span(" Uživatelé")),
        status = "primary", solidHeader = TRUE, width = 12, closable = FALSE,
        DT::DTOutput(ns("tbl")),
        footer = div(
          actionButton(ns("new"),     "Nový uživatel", icon = icon("user-plus"),
                       class = "btn btn-primary", style = "margin-right:6px"),
          actionButton(ns("resetpw"), "Reset hesla",   icon = icon("key"),
                       style = "margin-right:6px"),
          actionButton(ns("toggle"),  "Aktivovat/Deaktivovat", icon = icon("toggle-on"),
                       style = "margin-right:6px"),
          actionButton(ns("roles"),   "Nastavit role", icon = icon("id-badge"))
        )
      )
    )
  )
}


mod_admin_users_server <- function(id, pool, on_roles_changed = NULL){
  moduleServer(id, function(input, output, session){
    if (!requireNamespace("sodium", quietly = TRUE)) validate("Install 'sodium'")
    
    ns <- session$ns
    reload <- reactiveVal(0)
    users  <- reactive({ reload(); sql_list_users(pool) })
    
    output$tbl <- DT::renderDT({
      DT::datatable(
        users(),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 10, order = list(list(0, "asc"))),
        style = "bootstrap4"
      )
    }, server = TRUE)
    
    selected_user_id <- reactive({
      s <- input$tbl_rows_selected
      if (length(s)==1) users()$user_id[s] else NA
    })
    
    # --- Nový uživatel (modalDialog) ---
    observeEvent(input$new, {
      showModal(modalDialog(
        title = tagList(icon("user-plus"), span(" Nový uživatel")),
        textInput(ns("n_email"), "E-mail"),
        textInput(ns("n_name"),  "Jméno"),
        passwordInput(ns("n_pw"), "Heslo"),
        checkboxGroupInput(ns("n_roles"), "Role", choices = c("admin","user"), selected = "user"),
        footer = tagList(
          modalButton("Zrušit"),
          actionButton(ns("n_save"), "Uložit", class = "btn btn-primary", icon = icon("check"))
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$n_save, {
      req(input$n_email, input$n_name, input$n_pw)
      ph <- sodium::password_store(input$n_pw)
      try({
        uid <- sql_insert_user(pool,
                               tolower(trimws(input$n_email)),
                               input$n_name, ph,
                               roles = input$n_roles)
        reload(isolate(reload()) + 1)
        removeModal()
        showNotification(sprintf("Uživatel vytvořen (user_id=%s).", uid), type = "message")
      }, silent = TRUE)
    })
    
    # --- Reset hesla (modalDialog) ---
    observeEvent(input$resetpw, {
      uid <- selected_user_id(); req(!is.na(uid))
      showModal(modalDialog(
        title = tagList(icon("key"), span(" Reset hesla")),
        passwordInput(ns("r_pw"), "Nové heslo"),
        footer = tagList(
          modalButton("Zrušit"),
          actionButton(ns("r_save"), "Uložit", class = "btn btn-primary", icon = icon("check"))
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$r_save, {
      uid <- selected_user_id(); req(!is.na(uid), input$r_pw)
      ph <- sodium::password_store(input$r_pw)
      sql_reset_password(pool, uid, ph)
      removeModal()
      showNotification("Heslo změněno.", type = "message")
    })
    
    # --- Nastavit role (modalDialog) ---
    observeEvent(input$roles, {
      uid <- selected_user_id(); req(!is.na(uid))
      row <- users()[users()$user_id == uid, ]
      cur <- if (nrow(row) && !is.na(row$roles)) unlist(strsplit(row$roles, ",")) else character()
      showModal(modalDialog(
        title = tagList(icon("id-badge"), span(" Nastavit role")),
        HTML(sprintf("<p><b>Uživatel:</b> %s</p>", if (nrow(row)) row$email else "")),
        checkboxGroupInput(ns("rs_roles"), NULL, choices = c("admin","user"), selected = cur),
        footer = tagList(
          modalButton("Zrušit"),
          actionButton(ns("rs_save"), "Uložit", class = "btn btn-primary", icon = icon("check"))
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$rs_save, {
      uid <- selected_user_id(); req(!is.na(uid))
      sql_set_user_roles(pool, uid, input$rs_roles)
      removeModal()
      reload(isolate(reload()) + 1)
      if (!is.null(on_roles_changed)) try(on_roles_changed(uid), silent = TRUE)
    })
    
    # --- Aktivovat/Deaktivovat ---
    observeEvent(input$toggle, {
      uid <- selected_user_id(); req(!is.na(uid))
      row <- users()[users()$user_id == uid, ]
      sql_update_user_active(pool, uid, !isTRUE(row$is_active))
      reload(isolate(reload()) + 1)
    })
  })
}
