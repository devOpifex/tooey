t_enable_raw_mode <- function(auto_disable = TRUE) {
  .Call("enable_raw_mode_", as.logical(auto_disable))
}
t_disable_raw_mode <- function() {
  .Call("disable_raw_mode_")
}