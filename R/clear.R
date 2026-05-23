#' Clear the screen
#'
#' Clear the screen and move the cursor to the top left.
#'
#' @export
t_clear_screen <- function() {
  cat("\x1b[2J")
}