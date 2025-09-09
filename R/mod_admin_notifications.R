# R/mod_admin_notifications.R — správa notifikačních pravidel

mod_admin_notifications_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    box(
      title = tagList(icon("bell"), span(" Správa notifikací")),
      status = "primary", solidHeader = TRUE, width = 12,
      closable = FALSE, maximizable = FALSE,
      uiOutput(ns("rules_ui"))
    )
  )
}

mod_admin_notifications_server <- function(id, pool){
  moduleServer(id, function(input, output, session){
    rules <- reactiveVal(sql_get_notification_rules(pool))

    output$rules_ui <- renderUI({
      rs <- rules()
      if (nrow(rs) == 0) {
        "Žádná pravidla."
      } else {
        tagList(lapply(seq_len(nrow(rs)), function(i){
          checkboxInput(session$ns(rs$code[i]), rs$title[i], value = rs$is_active[i])
        }))
      }
    })

    observeEvent(rules(), {
      rs <- rules()
      for (i in seq_len(nrow(rs))){
        local({
          code <- rs$code[i]
          observeEvent(input[[code]], {
            sql_set_notification_rule_active(pool, code, isTRUE(input[[code]]))
          })
        })
      }
    }, once = TRUE)
  })
}
