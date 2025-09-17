#' Retrieve application configuration
#'
#' @return A list of configuration values coming from `golem-config.yml`.
get_golem_config <- function(config = c("default", "production")) {
  config <- match.arg(config)
  config::get(
    value = config,
    config = "golem-config.yml",
    package = "RBudgeting"
  )
}

#' Read database configuration
#'
#' @return A list containing database connection parameters.
get_db_config <- function() {
  cfg <- get_golem_config()
  cfg$db
}
