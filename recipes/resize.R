#! /usr/bin/env Rscript
devtools::load_all()

# Displays the current terminal size and updates live as you resize the window.
# Press q (or Ctrl-C) to quit.

init <- function(model) NULL

update <- function(model, msg) {
  if (is_key(msg, "q")) {
    return(list(model = model, cmd = cmd_quit()))
  }
  if (is_resize(msg)) {
    model$width <- msg@width
    model$height <- msg@height
    model$resizes <- model$resizes + 1L
    return(list(model = model, cmd = NULL))
  }
  list(model = model, cmd = NULL)
}

view <- function(model, buf) {
  buf <- t_text(
    buf,
    sprintf("Terminal: %d x %d", model$width, model$height),
    row = 1,
    col = 1
  )
  buf <- t_text(
    buf,
    sprintf("Resizes seen: %d", model$resizes),
    row = 2,
    col = 1,
    fg = "yellow"
  )
  t_text(
    buf,
    "resize the window; press q to quit",
    row = 4,
    col = 1,
    fg = "cyan"
  )
}

t <- Tooey(
  model = list(width = 0L, height = 0L, resizes = 0L),
  init = init,
  update = update,
  view = view
)
run(t)
