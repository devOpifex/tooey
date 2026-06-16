update <- S7::new_generic("update", "x")
S7::method(update, Tooey) <- function(x, f) {
  x@update <- f
  invisible(x)
}
