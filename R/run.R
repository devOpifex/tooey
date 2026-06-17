#' Run a Tooey application
#'
#' @param x A `Tooey` object.
#' @param ... Passed to methods.
#'
#' @export
run <- S7::new_generic("run", "x")
S7::method(run, Tooey) <- function(x) {
  t_enable_raw_mode()
  # Register teardown immediately, before entering the alternate screen, so a
  # failure mid-setup still restores the terminal. The restore escapes are
  # harmless no-ops if their matching enter never ran.
  on.exit(
    {
      t_show_cursor()
      t_leave_alt_screen()
      t_disable_raw_mode()
    },
    add = TRUE
  )
  t_enter_alt_screen()
  t_hide_cursor()
  t_clear_screen()
  t_poll(x)
}