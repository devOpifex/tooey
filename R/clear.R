#' Clear the screen
#'
#' Clear the screen and move the cursor to the top left.
#'
#' @export
t_clear_screen <- function() {
  e("2J")
}
# Switch to the terminal's alternate screen buffer (and back), so the app gets a
# clean canvas and the user's existing scrollback is restored untouched on exit.
t_enter_alt_screen <- function() {
  e("?1049h")
}
t_leave_alt_screen <- function() {
  e("?1049l")
}
# Hide the cursor while the app draws; show it again on exit.
t_hide_cursor <- function() {
  e("?25l")
}
t_show_cursor <- function() {
  e("?25h")
}