Tooey <- S7::new_class(
  "Tooey",
  package = "tooey",
  properties = list(
    model = S7::class_list,
    update = S7::class_function,
    view = S7::class_function,
    back = S7::class_atomic,
    front = Buffer,
    nrows = S7::class_integer,
    ncols = S7::class_integer
  ),
  constructor = function(model = list()) {
    # get_screen_dimensions() returns c(cols, rows); name them so the buffer
    # dimensions don't get transposed.
    dims <- get_screen_dimensions()
    cols <- dims[1]
    rows <- dims[2]
    S7::new_object(
      S7::S7_object(),
      back = matrix(NA_character_, nrow = rows, ncol = cols),
      front = Buffer(rows = rows, cols = cols),
      model = model,
      nrows = rows,
      ncols = cols,
      update = \(x) x,
      view = \() {}
    )
  }
)
