#' Sanitize
#'
#' Sanitize values to be compatible with warpscript.
#'
#' @param x An object to be sanitized.
#' @param return Type of object return, one of `iso8601` or `microsecond`.
#' @param ... Other parameters passed to method
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
sanitize <- function(x, ...) {
  UseMethod("sanitize", x)
}

#' @rdname sanitize
#' @export
sanitize.numeric <- function(x, ...) {
  return(x)
}

#' @rdname sanitize
#' @export
sanitize.list <- function(x, ...) {
  x <- x[!sapply(x, is.null)]
  if (!is.null(names(x))) {
    as.character(glue::glue("{{ {paste(sapply(names(x), sanitize), sapply(x, sanitize), collapse = \" \")} }}"))
  } else {
    as.character(glue::glue("[ {paste(sapply(x, sanitize), collapse = \" \")} ]"))
  }
}

#' @rdname sanitize
#' @export
sanitize.Duration <- function(x, ...) {
  paste(as.numeric(x), "s")
}

#' @rdname sanitize
#' @export
sanitize.POSIXct <- function(x, return = "iso8601") {
  if (return == "iso8601") {
    format_iso8601(x)
  } else if (return == "microsecond") {
    as.numeric(x) * 1e6
  }
}

#' @rdname sanitize
#' @export
sanitize.Date <- function(x, return = "iso8601") {
  if (return == "iso8601") {
    format_iso8601(x)
  } else if (return == "microsecond") {
    as.numeric(x) * 8.64e10
  }

}

#' @rdname sanitize
#' @export
sanitize.character <- function(x, return = "iso8601") {
  is_warpscript <- startsWith(x, "ws:")
  if (all(is_warpscript)) {
    return(as.character(gsub("^ws:", "", x)))
  }
  only_num <- grep("^[0-9]+$", x)
  if (length(only_num) == length(x)) {
    return(x)
  }
  timestamp <- anytime::anytime(x)
  if (!any(is.na(timestamp))) {
    return(sanitize(timestamp, return = return))
  }
  duration <- lubridate::as.duration(x)
  if (!any(is.na(duration))) {
    return(sanitize(duration))
  }
  boolean <- as.logical(x)
  if (!any(is.na(boolean))) {
    return(as.character(glue::glue("'{boolean}'")))
  }
  return(as.character(glue::glue("'{x}'")))
}

#' @rdname sanitize
#' @export
sanitize.NULL <- function(x, ...) {
  return(x)
}

#' @rdname sanitize
#' @export
sanitize.logical <- function(x, ...) {
  tolower(as.character(x))
}

#' @rdname sanitize
#' @export
sanitize.data.frame <- function(x, ...) {
  lapply(x, sanitize, ...)
}

format_iso8601 <- function(x) {
  as.character(glue::glue("'{paste0(anytime::iso8601(lubridate::as_datetime(x)), 'Z')}'"))
}
