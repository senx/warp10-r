#' Sanitize
#'
#' Sanitize values to be compatible with warpscript.
#'
#' @param x An object to be sanitized.
#' All character strings starting with `ws:` will be considered as valid warpscript.
#'
#' @export
#'
#' @examples
#' sanitize(
#'   list(
#'     num = list(1:10),
#'     char = "test",
#'     duration = "3 days",
#'     date = "2020-01-01",
#'     boolean = "true",
#'     warpscript = "ws:NOW"
#'   )
#' )
sanitize <- function(x) {
  UseMethod("sanitize", x)
}

#' @rdname sanitize
#' @export
sanitize.numeric <- function(x) {
  return(x)
}

#' @rdname sanitize
#' @export
sanitize.list <- function(x) {
  x <- x[!sapply(x, is.null)]
  if (!is.null(names(x))) {
    map <- paste(sapply(names(x), sanitize), sapply(x, sanitize), collapse = " ") # nolint
    as.character(glue::glue("{{ {map} }}"))
  } else {
    list <- paste(sapply(x, sanitize), collapse = " ") # nolint
    as.character(glue::glue("[ {list} ]"))
  }
}

#' @rdname sanitize
#' @export
sanitize.Duration <- function(x) {
  paste(as.numeric(x), "s")
}

#' @rdname sanitize
#' @export
sanitize.POSIXct <- function(x) {
  format_iso8601(x)
}

#' @rdname sanitize
#' @export
sanitize.Date <- function(x) {
  format_iso8601(x)
}

#' @rdname sanitize
#' @export
sanitize.character <- function(x) {
  is_warpscript <- startsWith(x, "ws:")
  if (is_warpscript) {
    return(as.character(gsub("^ws:", "", x)))
  }
  timestamp <- anytime::anytime(x)
  if (!is.na(timestamp)) {
    return(sanitize(timestamp))
  }
  duration <- lubridate::as.duration(x)
  if (!is.na(duration)) {
    return(sanitize(duration))
  }
  boolean <- as.logical(x)
  if (!is.na(boolean)) {
    return(as.character(glue::glue("'{boolean}'")))
  }
  return(as.character(glue::glue("'{x}'")))
}

#' @rdname sanitize
#' @export
sanitize.NULL <- function(x) {
  return(x)
}

#' @rdname sanitize
#' @export
sanitize.logical <- function(x) {
  tolower(as.character(x))
}

format_iso8601 <- function(x) {
  as.character(glue::glue("'{paste0(anytime::iso8601(lubridate::as_datetime(x)), 'Z')}'"))
}
