duration_to_time_unit <- function(x) {
  paste(as.numeric(x, units = "seconds"), "s")
}

parse_time_unit <- function(x) {
  UseMethod("parse_time_unit")
}

parse_time_unit.character <- function(x) {
  duration_to_time_unit(lubridate::as.duration(x))
}

parse_time_unit.numeric <- function(x) {
  x
}
