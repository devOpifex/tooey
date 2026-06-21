#' Render a frame
#'
#' Builds the next frame from the front buffer, diffs it against what is
#' already on screen, and writes only the cells that changed.
#'
#' @param x A `Tooey` object.
#' @param ... Passed to methods.
#'
#' @export
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

S7::method(render, Tooey) <- function(x) {
  con <- stdout()

  # Render every front-buffer cell down to the exact string we would emit to
  # the terminal (SGR styling + glyph + reset). Keeping the full escaped cell
  # here means the diff below catches colour and attribute changes, not just
  # changes to the visible character.
  new_frame <- matrix(NA_character_, nrow = x@nrows, ncol = x@ncols)
  for (i in seq_len(x@front@rows)) {
    for (j in seq_len(x@front@cols)) {
      char <- x@front@characters[i, j]
      fg <- x@front@foreground[i, j]
      bg <- x@front@background[i, j]
      attrs <- x@front@attributes[i, j]

      sgr <- t_sgr(fg, bg, attrs)
      if (nzchar(sgr)) {
        char <- paste0(sgr, char, "\x1b[0m")
      }

      new_frame[i, j] <- char
    }
  }

  # Diff against what is already on screen (`back`). Only cells that differ get
  # rewritten. The app always draws onto a freshly-cleared (blank) screen, so an
  # unknown (NA) back cell is really a blank space. Treating it as " " means the
  # first frame only emits the non-blank cells it actually draws, instead of
  # cursor-addressing every one of the rows*cols cells. That full repaint is a
  # huge single write (~tens of KB) which overflows the (non-blocking) terminal
  # and leaves the tail of the frame undrawn -- and because later frames only
  # emit diffs, those dropped cells are never repainted.
  prev <- x@back
  prev[is.na(prev)] <- " "
  changed <- which(new_frame != prev, arr.ind = TRUE)

  n <- nrow(changed)
  if (n) {
    out <- character(2L * n)
    for (k in seq_len(n)) {
      i <- changed[k, 1L]
      j <- changed[k, 2L]
      out[2L * k - 1L] <- sprintf("\x1b[%d;%dH", i, j)
      out[2L * k] <- new_frame[i, j]
    }

    cat(paste0(out, collapse = ""), file = con)
    flush(con)
  }

  # The new frame is now what's on screen: remember it for the next diff and
  # reset the front buffer so the next update starts from a clean slate.
  x@back <- new_frame
  x@front <- Buffer(rows = x@nrows, cols = x@ncols)
  invisible(x)
}
