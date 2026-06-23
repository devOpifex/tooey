#! /usr/bin/env Rscript
devtools::load_all()

# An interactive checklist, built entirely from the loop + key parser (no
# widgets). Move with up/down or j/k, jump with home/end, toggle an item with
# space or enter, and quit with q (or Ctrl-C).

choices <- c(
  "Carrots",
  "Celery",
  "Kohlrabi",
  "Fennel",
  "Aubergine"
)

init <- function(model) NULL

# Keep the cursor within the list bounds.
clamp <- function(i, n) max(1L, min(n, i))

update <- function(model, msg) {
  n <- length(model$choices)

  if (is_key(msg, "q") || is_key(msg, "ctrl-c")) {
    return(with_cmd(model, cmd_quit()))
  }
  if (is_key(msg, "up") || is_key(msg, "k")) {
    model$cursor <- clamp(model$cursor - 1L, n)
    return(with_cmd(model, NULL))
  }
  if (is_key(msg, "down") || is_key(msg, "j")) {
    model$cursor <- clamp(model$cursor + 1L, n)
    return(with_cmd(model, NULL))
  }
  if (is_key(msg, "home")) {
    model$cursor <- 1L
    return(with_cmd(model, NULL))
  }
  if (is_key(msg, "end")) {
    model$cursor <- n
    return(with_cmd(model, NULL))
  }
  # Space (a printable " ") or Enter toggles the item under the cursor.
  if (is_key(msg, " ") || is_key(msg, "enter")) {
    model$checked[model$cursor] <- !model$checked[model$cursor]
    return(with_cmd(model, NULL))
  }

  with_cmd(model, NULL)
}

view <- function(model, buf) {
  buf <- t_text(buf, "What shall we cook?", row = 1, col = 1, fg = "magenta")

  for (i in seq_along(model$choices)) {
    selected <- i == model$cursor
    cursor <- if (selected) ">" else " "
    box <- if (model$checked[i]) "[x]" else "[ ]"
    line <- sprintf("%s %s %s", cursor, box, model$choices[i])

    # Highlight the active row; otherwise show ticked items in green.
    if (selected) {
      buf <- t_text(buf, line, row = 2L + i, col = 1, fg = "black", bg = "cyan")
    } else if (model$checked[i]) {
      buf <- t_text(buf, line, row = 2L + i, col = 1, fg = "green")
    } else {
      buf <- t_text(buf, line, row = 2L + i, col = 1)
    }
  }

  help_row <- 4L + length(model$choices)
  n_checked <- sum(model$checked)
  buf <- t_text(
    buf,
    sprintf("%d selected", n_checked),
    row = help_row,
    col = 1,
    fg = "yellow"
  )
  buf <- t_text(
    buf,
    "up/down move - space toggle - q quit",
    row = help_row + 1L,
    col = 1,
    fg = "cyan"
  )
  buf
}

t <- Tooey(
  model = list(
    choices = choices,
    checked = rep(FALSE, length(choices)),
    cursor = 1L
  ),
  init = init,
  update = update,
  view = view
)
run(t)
