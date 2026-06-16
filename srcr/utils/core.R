p <- function(...) {
  cat(..., sep = "")
}

e <- function(...) {
  list(...) |>
    sapply(\(x) {
      paste0("\x1b[", x)
    }) |>
    p()
}

# Current time in seconds, for tick scheduling.
t_now <- function() {
  as.numeric(Sys.time())
}
