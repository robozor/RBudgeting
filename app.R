# app.R — Budgeting FE pro bs4Dash (>= 2.x)

suppressPackageStartupMessages({
  library(shiny); library(bs4Dash); library(shinyjs); library(DT)
})

# Tvrdý požadavek na verzi bs4Dash
if (utils::packageVersion("bs4Dash") < "2.0.0")
  stop("Tato aplikace vyžaduje bs4Dash >= 2.0.0. Prosím aktualizuj balíček `bs4Dash`.")

# Moduly & helpery
source("R/db.R")
source("R/mod_auth.R")
source("R/mod_admin_users.R")
source("R/mod_setup.R")
source("R/help.R")   # kontextová nápověda (markdown)

ui <- bs4DashPage(
  title  = "Budgeting – Shiny (bs4Dash)",
  
  header = bs4DashNavbar(
    title          = span("Budgeting", style = "font-weight:600"),
    controlbarIcon = icon("question-circle"),  # otazník = otevře pravý panel
    rightUi = tagList(
      # Vlastní Dark/Light přepínač
      tags$li(class = "dropdown",
              tags$a(id = "toggle_dark", href = "#", class = "nav-link",
                     title = "Dark/Light", icon("moon"))
      ),
      # Uživatelské menu (zatím 1 akce pro login/logout)
      bs4DropdownMenu(
        type = "notifications", icon = icon("user"), headerText = NULL,
        tags$a(id = "login_logout", href = "#",
               class = "dropdown-item action-button", "Přihlásit se")
      )
    )
  ),
  
  sidebar = bs4DashSidebar(
    skin  = "light",
    title = "Menu",
    bs4SidebarMenu(
      id = "sidebar_tabs",
      bs4SidebarMenuItem(
        "Plánování",
        icon = icon("calendar"),
        startExpanded = TRUE,
        bs4SidebarMenuSubItem("Obsah", tabName = "content", icon = icon("table"))
      ),
      bs4SidebarMenuItem(
        "Nastavení",
        icon = icon("cogs"),
        bs4SidebarMenuSubItem("Instalace systému", tabName = "setup", icon = icon("wrench")),
        bs4SidebarMenuSubItem("Přihlášení",        tabName = "login",  icon = icon("sign-in-alt")),
        bs4SidebarMenuSubItem("Správa uživatelů",  tabName = "admin",  icon = icon("users-cog"))
      )
    )
  ),
  
  body = bs4DashBody(
    useShinyjs(),
    
    # Skryj zbylé (nefunkční) theme přepínače
    tags$head(tags$style(HTML(" 
      .custom-control.custom-switch { display:none !important; }
    "))),
    
    # JS: Dark/Light přepínač s perzistencí + ikona moon/sun
    tags$script(HTML("
      (function(){
        var key='budgeting.darkmode';
        function applyState(){
          var v = localStorage.getItem(key);
          var dark = (v==='1');
          $('body').toggleClass('dark-mode', dark);
          var $i = $('#toggle_dark i');
          if ($i.length){
            $i.removeClass('fa-moon fa-sun').addClass(dark ? 'fa-sun' : 'fa-moon');
          }
          var $sidebar = $('aside.main-sidebar');
          if ($sidebar.length){
            $sidebar.removeClass('sidebar-dark-primary sidebar-light-primary')
                    .addClass(dark ? 'sidebar-dark-primary' : 'sidebar-light-primary');
          }
        }
        // Apply at start
        $(applyState);
        // Click handler
        $(document).on('click', '#toggle_dark', function(e){
          e.preventDefault();
          var dark = !$('body').hasClass('dark-mode');
          localStorage.setItem(key, dark ? '1' : '0');
          applyState();
        });
      })();
    ")),
    
    bs4TabItems(
      bs4TabItem(tabName = "setup",   mod_setup_ui("setup")),
      bs4TabItem(tabName = "login",   mod_auth_ui("auth")),
      bs4TabItem(tabName = "content", uiOutput("content_panel")),
      bs4TabItem(tabName = "admin",   mod_admin_users_ui("admin"))
    )
  ),
  
  # Pravý panel = kontextová nápověda (Markdown dle aktivní záložky)
  controlbar = bs4DashControlbar(
    id = "helpbar", title = "Nápověda", skin = "light", collapsed = TRUE,
    uiOutput("help_md")
  ),
  
  footer = NULL
)

server <- function(input, output, session){
  # 1) Instalační wizard (DB, metadata, admin)
  mod_setup_server("setup")
  
  # 2) Dostupnost DB/metadat
  pool_safe <- reactive({
    out <- try(get_db_pool(), silent = TRUE)
    if (inherits(out, "try-error")) NULL else out
  })
  app_ready <- reactive({
    p <- pool_safe(); if (is.null(p)) return(FALSE)
    isTRUE(try(metadata_exists(p), silent = TRUE))
  })
  
  # 3) Auth modul po app_ready
  auth_mod <- reactiveVal(NULL)
  observeEvent(app_ready(), {
    if (isTRUE(app_ready()) && is.null(auth_mod())) {
      auth_mod(mod_auth_server("auth", get_db_pool()))
    }
  }, ignoreInit = FALSE)
  
  current_user <- reactive({
    a <- auth_mod(); if (is.null(a)) return(NULL)
    a$user()
  })
  
  # 4) Routing podle stavu aplikace/uživatele
  observe({
    if (!isTRUE(app_ready())) {
      updatebs4TabItems(session, "sidebar_tabs", "setup")
    } else if (is.null(current_user())) {
      updatebs4TabItems(session, "sidebar_tabs", "login")
    } else {
      updatebs4TabItems(session, "sidebar_tabs", "content")
    }
  })
  
  # 5) Label „Přihlásit/odhlásit“ v uživatelském menu (dropdown)
  observe({
    lbl <- if (is.null(current_user())) "Přihlásit se" else "Odhlásit se"
    shinyjs::runjs(sprintf("$('#login_logout').text('%s')", lbl))
  })
  

  # 6) Klik na login/logout
  observeEvent(input$login_logout, {
    if (is.null(current_user())) {
      updatebs4TabItems(session, "sidebar_tabs", "login")
    } else {
      a <- auth_mod(); if (!is.null(a)) a$logout()
      updatebs4TabItems(session, "sidebar_tabs", "login")
    }
  })

  # 7) Obsah – info o uživateli
  output$content_panel <- renderUI({
    req(app_ready(), current_user())
    u <- current_user()
    box(
      title = tagList(icon("table"), span(" Obsah")),
      status = "primary", width = 12, solidHeader = TRUE, closable = FALSE, maximizable = FALSE,
      HTML(sprintf("<p><b>Přihlášený uživatel:</b> %s &lt;%s&gt;</p>", u$display_name, u$email)),
      HTML(sprintf("<p><b>Role:</b> %s</p>", paste(u$roles, collapse = ", ")))
    )
  })

  # 8) Admin modul jen pro adminy + refresh vlastních rolí po změně
  admin_inited <- reactiveVal(FALSE)
  observe({
    req(app_ready())
    u <- current_user()
    if (!is.null(u) && "admin" %in% u$roles && !admin_inited()) {
      mod_admin_users_server(
        "admin", get_db_pool(),
        on_roles_changed = function(uid){
          cu <- current_user()
          if (!is.null(cu) && cu$user_id == uid) {
            a <- auth_mod(); if (!is.null(a)) a$refresh()
            cu2 <- current_user()
            if (is.null(cu2) || !("admin" %in% cu2$roles)) {
              if (identical(input$sidebar_tabs, "admin"))
                updatebs4TabItems(session, "sidebar_tabs", "content")
            }
          }
        }
      )
      admin_inited(TRUE)
    }
  })

  # 9) Gate pro admin panel
  observe({
    req(app_ready())
    if (identical(input$sidebar_tabs, "admin") &&
        (is.null(current_user()) || !("admin" %in% current_user()$roles))) {
      updatebs4TabItems(session, "sidebar_tabs", "login")
    }
  })

  # 10) Kontextová nápověda – render podle aktivního tabName
  output$help_md <- renderUI({
    tab <- input$sidebar_tabs
    if (is.null(tab)) tab <- "index"
    get_help_md(tab)
  })
}

shinyApp(ui, server)
