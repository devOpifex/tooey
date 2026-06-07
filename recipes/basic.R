devtools::load_all()

i <- 0L

t <- Tooey()
t <- update(t, \(t) {
  i <<- i + 1L
  t <- t_text(t, sprintf("Frame %d", i))
  t
})
run(t)
