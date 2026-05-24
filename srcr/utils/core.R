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
