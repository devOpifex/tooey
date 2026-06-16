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
    dims <- get_screen_dimensions()
    S7::new_object(
      S7::S7_object(),
      back = matrix(NA_character_, nrow = dims[1], ncol = dims[2]),
      front = Buffer(rows = dims[1], cols = dims[2]),
      model = model,
      nrows = dims[1],
      ncols = dims[2],
      update = \(x) x,
      view = \() {}
    )
  }
)
