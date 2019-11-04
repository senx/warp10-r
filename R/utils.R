`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

labels_to_string <- function(labels) {
  if (!is.null(labels)) {
    paste(glue::glue("'{names(labels)}'"), glue::glue("'{labels}'"), collapse = " ")
  } else {
    ""
  }
}

# From https://stackoverflow.com/questions/34208564/how-to-hide-or-disable-in-function-printed-message/34208658#34208658
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  force(x)
}

format_iso8601 <- function(x) {
  sanitize(paste0(lubridate::format_ISO8601(lubridate::as_datetime(x)), "Z"))
}

sanitize <- function(x) {
  glue::glue("'{x}'")
}
