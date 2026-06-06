Tooey <- S7::new_class(
  "Tooey",
  package = "tooey",
  properties = list(
    model = S7::class_list,
    update = S7::class_function,
    view = S7::class_function,
    back = Buffer,
    front = Buffer
  ),
  constructor = function(model = list()) {
    dims <- get_screen_dimensions()
    S7::new_object(
      S7::S7_object(),
      back = Buffer(rows = dims[1], cols = dims[2]),
      front = Buffer(rows = dims[1], cols = dims[2]),
      model = model,
      update = \(x) x,
      view = \() {}
    )
  }
)