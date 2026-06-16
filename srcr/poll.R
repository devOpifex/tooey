t_poll <- S7::new_generic("t_poll", "x")

#' @export
S7::method(t_poll, Tooey) <- function(x) {
  con <- file("stdin", "rb", blocking = FALSE)
  on.exit(close(con), add = TRUE)

  model <- x@model

  # Run the startup command, then draw the initial frame once.
  next_tick <- Inf
  cmd <- x@init(model)
  if (S7::S7_inherits(cmd, TickCmd)) {
    next_tick <- t_now() + cmd@interval
  }
  quit <- S7::S7_inherits(cmd, QuitCmd)

  x@front <- x@view(model, Buffer(rows = x@nrows, cols = x@ncols))
  x <- render(x)

  repeat {
    if (quit) {
      break
    }

    # Collect pending messages: keyboard input first, then a tick if due.
    msgs <- list()

    bytes <- readBin(con, "raw", n = 16)
    if (length(bytes)) {
      msgs[[length(msgs) + 1L]] <- KeyMsg(key = t_parse_key(bytes))
    }

    if (t_now() >= next_tick) {
      msgs[[length(msgs) + 1L]] <- TickMsg()
      next_tick <- Inf
    }

    # Fold every message through update, then draw once. With no messages we
    # skip view/render entirely, so an idle app costs nothing.
    if (length(msgs)) {
      for (msg in msgs) {
        res <- x@update(model, msg)
        model <- res$model
        cmd <- res$cmd

        if (S7::S7_inherits(cmd, QuitCmd)) {
          quit <- TRUE
        }
        if (S7::S7_inherits(cmd, TickCmd)) {
          next_tick <- t_now() + cmd@interval
        }
      }

      x@model <- model
      x@front <- x@view(model, Buffer(rows = x@nrows, cols = x@ncols))
      x <- render(x)
    }

    Sys.sleep(0.005)
  }

  invisible(x)
}
