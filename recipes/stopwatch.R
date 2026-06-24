#! /usr/bin/env Rscript
library(tooey)

init <- function(model) cmd_tick(1 / 10)

update <- function(model, msg) {
  if (is_key(msg, "q") || is_key(msg, "ctrl-c")) {
    return(with_cmd(model, cmd_quit()))
  }
  if (is_key(msg, " ")) {
    if (model$running) {
      model$elapsed <- model$elapsed + (as.numeric(Sys.time()) - model$start)
      model$running <- FALSE
    } else {
      model$start <- as.numeric(Sys.time())
      model$running <- TRUE
    }
    return(with_cmd(model, NULL))
  }
  if (is_tick(msg)) {
    return(with_cmd(model, cmd_tick(1 / 10)))
  }
  with_cmd(model, NULL)
}

view <- function(model, buf) {
  total <- model$elapsed
  if (model$running) {
    total <- total + (as.numeric(Sys.time()) - model$start)
  }
  buf <- t_text(buf, sprintf("%.1fs", total), row = 1, col = 1, fg = "yellow")
  state <- if (model$running) "running" else "paused"
  buf <- t_text(buf, state, row = 2, col = 1, fg = "magenta")
  t_text(buf, "space pause/resume - q quit", row = 4, col = 1, fg = "cyan")
}

t <- Tooey(
  model = list(elapsed = 0, start = as.numeric(Sys.time()), running = TRUE),
  init = init,
  update = update,
  view = view
)
run(t)
