Buffer <- S7::new_class(
  "Buffer",
  properties = list(
    rows = S7::class_integer,
    cols = S7::class_integer,
    foreground = S7::class_atomic,
    background = S7::class_atomic,
    characters = S7::class_atomic,
    attributes = S7::class_atomic
  ),
  constructor = function(rows, cols) {
    if (missing(cols) || missing(rows)) {
      stop("rows and cols must be specified")
    }
    S7::new_object(
      S7::S7_object(),
      rows = rows,
      cols = cols,
      foreground = matrix("", nrow = rows, ncol = cols),
      background = matrix("", nrow = rows, ncol = cols),
      characters = matrix(" ", nrow = rows, ncol = cols),
      attributes = matrix(0L, nrow = rows, ncol = cols)
    )
  }
)