t_write_cell <- function(
  x,
  row,
  col,
  char,
  fg = "default",
  bg = "default",
  attrs = 0L
) {
  x@characters[row, col] <- char
  x@foreground[row, col] <- fg
  x@background[row, col] <- bg
  x@attributes[row, col] <- attrs
  x
}
