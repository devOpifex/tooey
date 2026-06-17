Msg <- S7::new_class("Msg", package = "tooey", abstract = TRUE)

#' Messages
#'
#' Messages are the values the runtime feeds to a `Tooey`'s `update` function.
#' A `KeyMsg` is emitted for a keypress; a `TickMsg` is emitted when a
#' [cmd_tick()] fires; a `WindowSizeMsg` is emitted once at startup and again
#' whenever the terminal is resized. Match them with [is_key()], [is_tick()],
#' and [is_resize()].
#'
#' @param key The key name, e.g. `"q"`, `"up"`, or `"enter"`.
#' @param width,height Terminal dimensions, in character cells.
#'
#' @name messages
#' @export
KeyMsg <- S7::new_class(
  "KeyMsg",
  package = "tooey",
  parent = Msg,
  properties = list(key = S7::class_character)
)

#' @rdname messages
#' @export
TickMsg <- S7::new_class("TickMsg", package = "tooey", parent = Msg)

#' @rdname messages
#' @export
WindowSizeMsg <- S7::new_class(
  "WindowSizeMsg",
  package = "tooey",
  parent = Msg,
  properties = list(width = S7::class_integer, height = S7::class_integer)
)

#' Is a message a (specific) keypress?
#'
#' @param msg A message.
#' @param key Optional key name to match (e.g. `"q"`, `"up"`).
#'
#' @export
is_key <- function(msg, key = NULL) {
  if (!S7::S7_inherits(msg, KeyMsg)) {
    return(FALSE)
  }
  if (is.null(key)) {
    return(TRUE)
  }
  identical(msg@key, key)
}

#' Is a message a tick?
#'
#' @param msg A message.
#'
#' @export
is_tick <- function(msg) {
  S7::S7_inherits(msg, TickMsg)
}

#' Is a message a terminal resize?
#'
#' @param msg A message.
#'
#' @export
is_resize <- function(msg) {
  S7::S7_inherits(msg, WindowSizeMsg)
}
