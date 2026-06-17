t_poll <- S7::new_generic("t_poll", "x")

#' @export
S7::method(t_poll, Tooey) <- function(x) {
  con <- file("stdin", "rb", blocking = FALSE)
  on.exit(close(con), add = TRUE)

  model <- x@model
  next_tick <- Inf
  quit <- FALSE

  # Drain a queue of messages through `update`. Each message may yield a
  # command, which the runtime runs immediately; any messages it produces are
  # appended to the queue and folded through `update` in the same pass. Quit
  # and tick requests are applied to the enclosing loop state. Returns the new
  # model and whether anything was processed (so the caller knows to redraw).
  dispatch <- function(model, queue) {
    changed <- FALSE
    i <- 1L
    while (i <= length(queue)) {
      msg <- queue[[i]]
      i <- i + 1L

      res <- x@update(model, msg)
      model <- res$model
      changed <- TRUE

      eff <- t_run_cmd(res$cmd)
      if (eff$quit) {
        quit <<- TRUE
      }
      if (!is.null(eff$tick)) {
        next_tick <<- t_now() + eff$tick
      }
      if (length(eff$msgs)) {
        queue <- c(queue, eff$msgs)
      }
    }
    list(model = model, changed = changed)
  }

  # Run the startup command, dispatching any messages it produces, then draw
  # the initial frame once.
  eff <- t_run_cmd(x@init(model))
  if (eff$quit) {
    quit <- TRUE
  }
  if (!is.null(eff$tick)) {
    next_tick <- t_now() + eff$tick
  }
  if (length(eff$msgs)) {
    model <- dispatch(model, eff$msgs)$model
  }

  x@model <- model
  x@front <- x@view(model, Buffer(rows = x@nrows, cols = x@ncols))
  x <- render(x)

  repeat {
    if (quit) {
      break
    }

    # Collect pending messages: keyboard input first, then a tick if due.
    queue <- list()

    bytes <- readBin(con, "raw", n = 16)
    if (length(bytes)) {
      queue[[length(queue) + 1L]] <- KeyMsg(key = t_parse_key(bytes))
    }

    if (t_now() >= next_tick) {
      queue[[length(queue) + 1L]] <- TickMsg()
      next_tick <- Inf
    }

    # Fold every message through update, then draw once. With no messages we
    # skip view/render entirely, so an idle app costs nothing.
    if (length(queue)) {
      res <- dispatch(model, queue)
      model <- res$model
      if (res$changed) {
        x@model <- model
        x@front <- x@view(model, Buffer(rows = x@nrows, cols = x@ncols))
        x <- render(x)
      }
    }

    Sys.sleep(0.005)
  }

  invisible(x)
}
