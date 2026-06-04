run <- S7::new_generic("run", "x")
#' @export
S7::method(run, Tooey) <- function(x) {
  t_enable_raw_mode()
  t_clear_screen()
  t_poll(x)
}