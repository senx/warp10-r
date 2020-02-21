`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}

# From https://stackoverflow.com/questions/34208564/how-to-hide-or-disable-in-function-printed-message/34208658#34208658
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  force(x)
}

first <- dplyr::first
