# Translate raw stdin bytes into a key name. Best-effort and extensible:
# printable bytes map to themselves; common control bytes and the arrow-key
# escape sequences get friendly names.
t_parse_key <- function(bytes) {
  if (!length(bytes)) {
    return(NULL)
  }
  codes <- as.integer(bytes)
  # Arrow keys arrive as ESC [ A/B/C/D.
  if (length(codes) >= 3 && codes[1] == 27 && codes[2] == 91) {
    arrow <- switch(
      intToUtf8(codes[3]),
      A = "up",
      B = "down",
      C = "right",
      D = "left"
    )
    if (!is.null(arrow)) {
      return(arrow)
    }
  }
  switch(
    as.character(codes[1]),
    "27" = "esc",
    "3" = "ctrl-c",
    "13" = "enter",
    "10" = "enter",
    "127" = "backspace",
    rawToChar(bytes[1])
  )
}