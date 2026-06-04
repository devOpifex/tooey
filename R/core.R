Tooey <- S7::new_class(
  "Tooey",
  package = "tooey",
  properties = list(
    buffer = Buffer
  ),
  constructor = function() {
    dims <- get_screen_dimensions()
    S7::new_object(
      S7::S7_object(),
      buffer = Buffer(rows = dims[1], cols = dims[2])
    )
  }
)