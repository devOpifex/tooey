Cmd <- S7::new_class("Cmd", package = "tooey", abstract = TRUE)

QuitCmd <- S7::new_class("QuitCmd", package = "tooey", parent = Cmd)

TickCmd <- S7::new_class(
  "TickCmd",
  package = "tooey",
  parent = Cmd,
  properties = list(interval = S7::class_numeric)
)

#' Tell the runtime to quit.
#'
#' @export
cmd_quit <- function() {
  QuitCmd()
}

#' Tell the runtime to send a tick message after `interval` seconds.
#'
#' @param interval Seconds until the tick fires.
#'
#' @export
cmd_tick <- function(interval) {
  TickCmd(interval = interval)
}
