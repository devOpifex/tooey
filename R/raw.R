t_enable_raw_mode <- function() {
  .Call("enable_raw_mode_")
}
t_disable_raw_mode <- function(auto_disable = TRUE) {
  .Call("disable_raw_mode_", as.integer(auto_disable))
}