#' @useDynLib tooey, .registration = TRUE
NULL
.onLoad <- function(...) {
  S7::methods_register()
}