#' Create a Tooey application
#'
#' @param model Initial application state (a list).
#' @param init Function `(model) -> cmd`; a startup command, or `NULL`.
#' @param update Function `(model, msg) -> list(model = , cmd = )`.
#' @param view Function `(model, buf) -> buf`; draws the model into `buf`.
#'
#' @export
Tooey <- S7::new_class(
  "Tooey",
  package = "tooey",
  properties = list(
    model = S7::class_list,
    init = S7::class_function,
    update = S7::class_function,
    view = S7::class_function,
    back = S7::class_atomic,
    front = Buffer,
    nrows = S7::class_integer,
    ncols = S7::class_integer
  ),
  constructor = function(
    model = list(),
    init = function(model) NULL,
    update = function(model, msg) list(model = model, cmd = NULL),
    view = function(model, buf) buf
  ) {
    # get_screen_dimensions() returns c(cols, rows); name them so the buffer
    # dimensions don't get transposed.
    dims <- get_screen_dimensions()
    cols <- dims[1]
    rows <- dims[2]

    S7::new_object(
      S7::S7_object(),
      model = model,
      init = init,
      update = update,
      view = view,
      back = matrix(NA_character_, nrow = rows, ncol = cols),
      front = Buffer(rows = rows, cols = cols),
      nrows = rows,
      ncols = cols
    )
  }
)

# Resize the runtime's buffers to new terminal dimensions. The back buffer is
# reset to all-NA so the next render() treats every cell as changed and repaints
# the whole screen at the new size.
t_resize <- S7::new_generic("t_resize", "x")
S7::method(t_resize, Tooey) <- function(x, cols, rows) {
  x@ncols <- cols
  x@nrows <- rows
  x@front <- Buffer(rows = rows, cols = cols)
  x@back <- matrix(NA_character_, nrow = rows, ncol = cols)
  x
}
