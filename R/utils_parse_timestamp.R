format_iso8601 <- function(x) {
  sanitize(paste0(anytime::iso8601(lubridate::as_datetime(x)), "Z"))
}

parse_timestamp <- function(x) {
  UseMethod("parse_timestamp")
}

parse_timestamp.Date <- function(x) {
  format_iso8601(x)
}

parse_timestamp.POSIXct <- function(x) {
  format_iso8601(x)
}

parse_timestamp.character <- function(x) {
  if (x == "NOW") return(x)
  timestamp <- anytime::anytime(x)
  if (is.na(timestamp)) {
    msg <- glue::glue("Could not parse '{x}' to a proper timestamp.")
    stop(msg, call. = FALSE)
  }
  format_iso8601(x)
}

parse_timestamp.numeric <- function(x) {
  return(x)
}

parse_timestamp.Duration <- function(x) {
  paste(as.numeric(x), "s")
}
