devtools::load_all()

items <- c("About", "Settings", "Quit")

clamp <- function(i, n) max(1L, min(n, i))

init <- function(model) NULL

update <- function(model, msg) {
  if (is_key(msg, "ctrl-c")) {
    return(with_cmd(model, cmd_quit()))
  }

  if (model$screen != "menu") {
    if (is_key(msg, "esc") || is_key(msg, "enter")) {
      model$screen <- "menu"
    }
    return(with_cmd(model, NULL))
  }

  n <- length(model$items)
  if (is_key(msg, "up") || is_key(msg, "k")) {
    model$cursor <- clamp(model$cursor - 1L, n)
    return(with_cmd(model, NULL))
  }
  if (is_key(msg, "down") || is_key(msg, "j")) {
    model$cursor <- clamp(model$cursor + 1L, n)
    return(with_cmd(model, NULL))
  }
  if (is_key(msg, "enter")) {
    choice <- model$items[model$cursor]
    if (choice == "Quit") {
      return(with_cmd(model, cmd_quit()))
    }
    model$screen <- tolower(choice)
    return(with_cmd(model, NULL))
  }
  with_cmd(model, NULL)
}

view_menu <- function(model, buf) {
  buf <- t_text(buf, "Main menu", row = 1, col = 1, fg = "magenta")
  for (i in seq_along(model$items)) {
    selected <- i == model$cursor
    line <- sprintf("%s %s", if (selected) ">" else " ", model$items[i])
    if (selected) {
      buf <- t_text(buf, line, row = 1L + i, col = 1, fg = "black", bg = "cyan")
    } else {
      buf <- t_text(buf, line, row = 1L + i, col = 1)
    }
  }
  t_text(buf, "up/down move - enter select - ctrl-c quit", row = 3L + length(model$items), col = 1, fg = "cyan")
}

view <- function(model, buf) {
  if (model$screen == "menu") {
    return(view_menu(model, buf))
  }
  body <- switch(
    model$screen,
    about = "tooey - a TUI framework for R.",
    settings = "Nothing to configure yet."
  )
  buf <- t_text(buf, toupper(model$screen), row = 1, col = 1, fg = "magenta")
  buf <- t_text(buf, body, row = 3, col = 1)
  t_text(buf, "esc back", row = 5, col = 1, fg = "cyan")
}

t <- Tooey(
  model = list(items = items, cursor = 1L, screen = "menu"),
  init = init,
  update = update,
  view = view
)
run(t)
