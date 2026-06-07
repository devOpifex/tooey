render <- S7::new_generic("render", "x")

# Basic 8-colour palette mapped to their ANSI SGR offsets.
.t_colors <- c(
  black = 0L,
  red = 1L,
  green = 2L,
  yellow = 3L,
  blue = 4L,
  magenta = 5L,
  cyan = 6L,
  white = 7L
)

# Translate a colour name to an SGR code, or NULL to leave the terminal default.
t_color_code <- function(x, offset) {
  if (is.na(x) || !nzchar(x) || x == "default") {
    return(NULL)
  }
  i <- .t_colors[x]
  if (is.na(i)) {
    return(NULL)
  }
  offset + unname(i)
}

# Build the SGR escape (`\x1b[...m`) for a cell, or "" when nothing overrides
# the terminal defaults (so blank cells stay cheap and uncoloured).
t_sgr <- function(fg, bg, attrs) {
  codes <- integer(0)
  if (!is.na(attrs) && attrs != 0L) {
    codes <- c(codes, attrs)
  }
  codes <- c(codes, t_color_code(fg, 30L), t_color_code(bg, 40L))
  if (!length(codes)) {
    return("")
  }
  sprintf("\x1b[%sm", paste(codes, collapse = ";"))
}

#' @export
S7::method(render, Tooey) <- function(x) {
  con <- file("stdout", "wb", blocking = FALSE)
  on.exit(
    {
      close(con)
      unlink("stdout")
    },
    add = TRUE
  )

  out <- character(0)
  for (i in seq_len(x@front@rows)) {
    # Position the cursor at the start of this row so each frame overwrites
    # the previous one in place instead of scrolling.
    out <- c(out, sprintf("\x1b[%d;1H", i))

    for (j in seq_len(x@front@cols)) {
      char <- x@front@characters[i, j]
      fg <- x@front@foreground[i, j]
      bg <- x@front@background[i, j]
      attrs <- x@front@attributes[i, j]

      sgr <- t_sgr(fg, bg, attrs)
      if (nzchar(sgr)) {
        out <- c(out, sgr, char, "\x1b[0m")
      } else {
        out <- c(out, char)
      }
    }
  }

  writeBin(charToRaw(paste0(out, collapse = "")), con)
  flush(con)
}
