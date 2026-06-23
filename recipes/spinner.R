devtools::load_all()

frames <- c("⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏")

init <- function(model) cmd_tick(1 / 12)

update <- function(model, msg) {
  if (is_key(msg, "q") || is_key(msg, "ctrl-c")) {
    return(with_cmd(model, cmd_quit()))
  }
  if (is_tick(msg)) {
    model$frame <- model$frame + 1L
    return(with_cmd(model, cmd_tick(1 / 12)))
  }
  with_cmd(model, NULL)
}

view <- function(model, buf) {
  glyph <- model$frames[(model$frame %% length(model$frames)) + 1L]
  buf <- t_text(buf, sprintf("%s Working...", glyph), row = 1, col = 1, fg = "magenta")
  t_text(buf, "press q to quit", row = 3, col = 1, fg = "cyan")
}

t <- Tooey(
  model = list(frame = 0L, frames = frames),
  init = init,
  update = update,
  view = view
)
run(t)
