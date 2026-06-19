<!-- badges: start -->
<!-- badges: end -->

# tooey

Terminal User Interface (TUI) framework for R.

## Installation

``` r
# install.packages("pak")
pak::pak("devOpifex/tooey")
```

## Example

``` r
library(tooey)

init <- function(model) cmd_tick(1 / 30) # startup: begin the clock

update <- function(model, msg) {
  if (is_key(msg, "q")) {
    return(with_cmd(model, cmd_quit()))
  }
  if (is_tick(msg)) {
    model$frame <- model$frame + 1L
    return(with_cmd(model = model, cmd = cmd_tick(1 / 30))) # schedule next tick
  }
  with_cmd(model = model, cmd = NULL)
}

view <- function(model, buf) {
  buf <- t_text(buf, sprintf("Frame %d", model$frame), row = 1, col = 1)
  buf <- t_text(buf, "press q to quit", row = 3, col = 1, fg = "cyan")
  buf
}

t <- Tooey(model = list(frame = 0L), init = init, update = update, view = view)
run(t)
```

