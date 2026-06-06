devtools::load_all()

t <- Tooey()
t <- update(t, \(t) {
  t <- t_text(t, "Hello World!")
  t
})
run(t)
