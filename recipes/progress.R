#! /usr/bin/env Rscript
devtools::load_all()

init <- function(model) cmd_tick(1 / 30)

update <- function(model, msg) {
  if (is_key(msg, "q")) {
    return(with_cmd(model, cmd_quit()))
  }

  if (!is_tick(msg)) {
    return(with_cmd(model = model, cmd = cmd_tick(1 / 30)))
  }

  model$tick <- model$tick + 1L

  with_cmd(model = model, cmd = cmd_tick(1 / 30))
}

view <- function(model, buf) {
  width <- buf@cols
  pct <- min(model$tick, 100L)
  filled <- as.integer(round(pct / 100 * width))
  buf <- t_text(buf, strrep("█", filled), row = 1L, col = 1L)
  if (filled < width) {
    buf <- t_text(buf, strrep("░", width - filled), row = 1L, col = filled + 1L)
  }
  buf <- t_text(buf, sprintf("%d%%", pct), row = 2L, col = 1L)
  buf <- t_text(buf, "press q to quit", row = 3L, col = 1L, fg = "cyan")
  buf
}

t <- Tooey(model = list(tick = 0L), init = init, update = update, view = view)
run(t)
