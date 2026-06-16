#' Run a Tooey application
#'
#' @param x A `Tooey` object.
#' @param ... Passed to methods.
#'
#' @export
run <- S7::new_generic("run", "x")
S7::method(run, Tooey) <- function(x) {
  t_enable_raw_mode()
  on.exit(t_disable_raw_mode(), add = TRUE)
  t_clear_screen()
  t_poll(x)
}