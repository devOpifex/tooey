#'  Write text
#'
#' @param x A `Tooey` or `Buffer` object.
#' @param ... Passed to methods.
#'
#' @export
t_text <- S7::new_generic("t_text", "x")
S7::method(t_text, Tooey) <- function(
  x,
  text,
  row = 1L,
  col = 1L,
  fg = "default",
  bg = "default"
) {
  x@front <- t_text(x@front, text, row, col, fg, bg)
  invisible(x)
}
S7::method(t_text, Buffer) <- function(
  x,
  text,
  row = 1L,
  col = 1L,
  fg = "default",
  bg = "default"
) {
  if (missing(text)) {
    stop("text must be specified")
  }
  chars <- strsplit(text, "")[[1]]
  for (i in seq_along(chars)) {
    x <- t_write_cell(x, row, col + i - 1, chars[i], fg, bg)
  }
  invisible(x)
}