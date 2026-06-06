t_poll <- S7::new_generic("t_poll", "x")

#' @export
S7::method(t_poll, Tooey) <- function(x) {
  con <- file("stdin", "rb", blocking = FALSE)
  on.exit(close(con), add = TRUE)

  repeat {
    bytes <- readBin(con, "raw", n = 16)

    t <- x@update(x)

    if (length(bytes) && rawToChar(bytes) == "q") {
      p("\x1b[1;1H\x1b[31mquitting...\x1b[0m\n")
      t_q()
    }

    render(t)

    # TODO: handle FPS
    Sys.sleep(0.005)
  }
}
