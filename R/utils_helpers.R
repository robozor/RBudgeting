#' Access application files
#'
#' @param ... Path elements relative to the package root.
#' @return A file path.
app_sys <- function(...) {
  system.file(..., package = "RBudgeting")
}

#' Add a notification to the navbar menu
#'
#' @param session Shiny session object
#' @param text Notification text
#' @param status Status color (info, primary, danger, ...)
#' @param icon Font Awesome icon name
add_notification <- function(session, text, status = "info", icon = "info-circle") {
  if (is.null(session$userData$notifications)) {
    return(invisible(FALSE))
  }
  notif_rv <- session$userData$notifications
  current <- notif_rv()
  notif_rv(c(list(list(text = text, status = status, icon = icon)), current))
  invisible(TRUE)
}
