<!-- badges: start -->
<!-- badges: end -->

# tooey

Terminal User Interface (TUI) framework for R.

:warning: This package is still in development.
The code is written by hand, an LLM was used for the input parser.

## Installation

``` r
# install.packages("pak")
pak::pak("devOpifex/tooey")
```

## Example

See `recipes/` for more examples.

``` r
library(tooey)

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
```

