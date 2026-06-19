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
      changed <- TRUE
      cmd <- NULL
      model <- NULL
      # update() may return WithCmd, a Cmd, or the model.
      if (is_with_cmd(res)) {
        model <- res@model
        cmd <- res@cmd
      }
      if (is_cmd(res)) {
        cmd <- res
      }
      if (is.list(res)) {
        model <- res$model
      }
      eff <- t_run_cmd(cmd)
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
  # the initial frame once. An initial WindowSizeMsg goes first, so `view`
  # always has the terminal dimensions before the app's own startup messages.
  eff <- t_run_cmd(x@init(model))
  if (eff$quit) {
    quit <- TRUE
  }
  if (!is.null(eff$tick)) {
    next_tick <- t_now() + eff$tick
  }
  startup <- c(
    list(WindowSizeMsg(width = x@ncols, height = x@nrows)),
    eff$msgs
  )
  model <- dispatch(model, startup)$model
  x@model <- model
  x@front <- x@view(model, Buffer(rows = x@nrows, cols = x@ncols))
  x <- render(x)
  repeat {
    if (quit) {
      break
    }
    # Collect pending messages: keyboard input first, then a tick if due.
    queue <- list()
    interrupt <- FALSE
    bytes <- readBin(con, "raw", n = 16)
    if (length(bytes)) {
      key <- t_parse_key(bytes)
      queue[[length(queue) + 1L]] <- KeyMsg(key = key)
      # Ctrl-C is a runtime-level escape hatch: dispatch it so `update` can
      # observe it, render the resulting frame, then quit no matter what.
      interrupt <- identical(key, "ctrl-c")
    }
    if (t_now() >= next_tick) {
      queue[[length(queue) + 1L]] <- TickMsg()
      next_tick <- Inf
    }
    # Poll the terminal size (a cheap ioctl). On change, resize the buffers and
    # tell the app, so `view` can re-lay-out and render() repaints in full.
    dims <- get_screen_dimensions()
    if (dims[1] != x@ncols || dims[2] != x@nrows) {
      x <- t_resize(x, cols = dims[1], rows = dims[2])
      queue[[length(queue) + 1L]] <- WindowSizeMsg(
        width = x@ncols,
        height = x@nrows
      )
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
    if (interrupt) {
      quit <- TRUE
    }
    Sys.sleep(0.005)
  }
  invisible(x)
}