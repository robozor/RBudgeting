# R/help.R — mapování a loader kontextové nápovědy

if (!requireNamespace("yaml", quietly = TRUE)) stop("Install 'yaml'")

HELP_MAP_FILE <- "help/help_map.yml"

# Defaultní mapa, pokud YAML chybí
.default_help_map <- function(){
  list(
    setup   = "help/setup.md",
    login   = "help/login.md",
    content = "help/content.md",
    admin   = "help/admin.md",
    index   = "help/index.md"
  )
}

load_help_map <- function(){
  if (file.exists(HELP_MAP_FILE)) {
    m <- yaml::read_yaml(HELP_MAP_FILE)
    # sloučíme s defaulty (aby nechyběl index atd.)
    defaults <- .default_help_map()
    for (k in names(defaults)) if (is.null(m[[k]])) m[[k]] <- defaults[[k]]
    m
  } else {
    .default_help_map()
  }
}

get_help_md <- function(tab_name){
  m <- load_help_map()
  path <- m[[tab_name]]
  if (is.null(path)) path <- m$index
  if (!file.exists(path)) return(htmltools::HTML("<em>Nápověda pro tuto stránku zatím není k dispozici.</em>"))
  includeMarkdown(path)
}
