#! /usr/bin/env Rscript
devtools::load_all()

init <- function(model) NULL

update <- function(model, msg) {
  if (is_key(msg, "ctrl-c") || is_key(msg, "esc")) {
    return(with_cmd(model, cmd_quit()))
  }
  if (is_key(msg, "enter")) {
    model$submitted <- model$text
    model$text <- ""
    return(with_cmd(model, NULL))
  }
  if (is_key(msg, "backspace")) {
    model$text <- substr(model$text, 1L, max(0L, nchar(model$text) - 1L))
    return(with_cmd(model, NULL))
  }
  if (is_key(msg) && nchar(msg@key) == 1L) {
    model$text <- paste0(model$text, msg@key)
    return(with_cmd(model, NULL))
  }
  with_cmd(model, NULL)
}

view <- function(model, buf) {
  buf <- t_text(buf, "Type something:", row = 1, col = 1, fg = "magenta")
  buf <- t_text(buf, paste0(model$text, "_"), row = 2, col = 1, fg = "yellow")
  if (nzchar(model$submitted)) {
    buf <- t_text(
      buf,
      sprintf("You said: %s", model$submitted),
      row = 4,
      col = 1,
      fg = "green"
    )
  }
  t_text(buf, "enter submit - esc quit", row = 6, col = 1, fg = "cyan")
}

t <- Tooey(
  model = list(text = "", submitted = ""),
  init = init,
  update = update,
  view = view
)
run(t)
