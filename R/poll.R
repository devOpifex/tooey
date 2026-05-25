t_poll <- function(timeout_ms = 16) {
  deadline <- Sys.time() + timeout_ms / 1000
  con <- file("stdin", "rb", blocking = FALSE)
  on.exit(close(con), add = TRUE)
  repeat {
    bytes <- readBin(con, "raw", n = 16)
    if (length(bytes) <= 0) {
      next
    }
    if (length(bytes) && rawToChar(bytes) == "q") {
      p("\x1b[5;10H\x1b[31mquitting...\x1b[0m\n")
      t_q()
    }
    Sys.sleep(0.005) # 5ms — tradeoff between responsiveness and CPU
  }
}