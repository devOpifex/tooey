Cmd <- S7::new_class("Cmd", package = "tooey", abstract = TRUE)
QuitCmd <- S7::new_class("QuitCmd", package = "tooey", parent = Cmd)
TickCmd <- S7::new_class(
  "TickCmd",
  package = "tooey",
  parent = Cmd,
  properties = list(interval = S7::class_numeric)
)
# A command wrapping an arbitrary effect. The runtime calls `fn`; whatever
# message it returns is fed back through `update`, and `NULL` dispatches
# nothing. This is the general-purpose escape hatch: read a file, call an API,
# run a computation, then turn the result into a message.
FunCmd <- S7::new_class(
  "FunCmd",
  package = "tooey",
  parent = Cmd,
  properties = list(fn = S7::class_function)
)
# A command that fans out into several commands. The runtime runs each in turn,
# collecting every message they produce.
BatchCmd <- S7::new_class(
  "BatchCmd",
  package = "tooey",
  parent = Cmd,
  properties = list(cmds = S7::class_list)
)
#' Commands
#'
#' Commands are how `update` asks the runtime to *do* something: quit, schedule
#' a tick, run an effect, or several at once. `update` returns a command (or
#' `NULL`) alongside the new model; the runtime carries it out and feeds any
#' resulting message back through `update`.
#'
#' - [cmd_quit()] stops the application.
#' - [cmd_tick()] asks for a [TickMsg] after `interval` seconds.
#' - [cmd_run()] runs an arbitrary effect `() -> msg`.
#' - [cmd_msg()] dispatches a message immediately.
#' - [cmd_batch()] combines several commands into one.
#'
#' Effects run synchronously on the runtime loop, so a long-running [cmd_run()]
#' blocks input and redraws until it returns.
#'
#' @param interval Seconds until the tick fires.
#' @param fn A function `() -> msg`. Its return value, unless `NULL`, is fed
#'   back through `update` as a message.
#' @param msg A message to dispatch.
#' @param ... Commands to combine; `NULL`s are dropped.
#'
#' @name commands
NULL
#' @rdname commands
#' @export
cmd_quit <- function() {
  QuitCmd()
}
#' @rdname commands
#' @export
cmd_tick <- function(interval) {
  TickCmd(interval = interval)
}
#' @rdname commands
#' @export
cmd_run <- function(fn) {
  if (!is.function(fn)) {
    stop("`fn` must be a function `() -> msg`")
  }
  FunCmd(fn = fn)
}
#' @rdname commands
#' @export
cmd_msg <- function(msg) {
  force(msg)
  FunCmd(fn = function() msg)
}
#' @rdname commands
#' @export
cmd_batch <- function(...) {
  cmds <- Filter(Negate(is.null), list(...))
  # Bare functions are accepted as commands; normalise them so the batch is a
  # homogeneous list of Cmd objects.
  cmds <- lapply(cmds, function(cmd) {
    if (is.function(cmd)) cmd_run(cmd) else cmd
  })
  if (!length(cmds)) {
    return(NULL)
  }
  if (length(cmds) == 1L) {
    return(cmds[[1L]])
  }
  BatchCmd(cmds = cmds)
}
# Effects record: messages to feed back through `update`, whether to quit, and
# the seconds until the next tick (NULL = leave the schedule unchanged).
t_effects <- function(msgs = list(), quit = FALSE, tick = NULL) {
  list(msgs = msgs, quit = quit, tick = tick)
}
# Interpret a command into runtime effects. A command may be NULL, a bare
# function `() -> msg`, or any Cmd; batches are flattened recursively.
t_run_cmd <- S7::new_generic("t_run_cmd", "cmd")
# update usually returns no command, so NULL is the hot path.
S7::method(t_run_cmd, NULL) <- function(cmd) {
  t_effects()
}
# Bare functions are accepted as commands, Bubble-Tea style.
S7::method(t_run_cmd, S7::class_function) <- function(cmd) {
  t_run_cmd(cmd_run(cmd))
}
S7::method(t_run_cmd, QuitCmd) <- function(cmd) {
  t_effects(quit = TRUE)
}
S7::method(t_run_cmd, TickCmd) <- function(cmd) {
  t_effects(tick = cmd@interval)
}
S7::method(t_run_cmd, FunCmd) <- function(cmd) {
  msg <- cmd@fn()
  t_effects(msgs = if (is.null(msg)) list() else list(msg))
}
# Run each child in turn, collecting every message; the last tick wins.
S7::method(t_run_cmd, BatchCmd) <- function(cmd) {
  eff <- t_effects()
  for (child in cmd@cmds) {
    e <- t_run_cmd(child)
    eff$msgs <- c(eff$msgs, e$msgs)
    eff$quit <- eff$quit || e$quit
    if (!is.null(e$tick)) {
      eff$tick <- e$tick
    }
  }
  eff
}