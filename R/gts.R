#' Coersce object to GTS tibbles
#'
#' @description
#'
#' `as_gts()` turns an existing object, such as a data frame into
#' an extended tibble with class ['gts'], and attributes corresponding
#' to the GTS attributes + class.
#' A [`GTS`] object is basically a data frame with some information about the
#' timeserie.
#' The resulting tibble contains at least two columns: `timestamp` and `value`.
#'
#' `as_gts()` is an S3 generic.
#'
#' @param x An object that could reasoably be coerced to a gts tibble.
#' @param class The class of the GTS.
#' @param labels A named list of labels in the form `key = value`.
#'
#' @export
#'
#' @examples
#'
#' x <- data.frame(timestamp = 1:10, value = rnorm(10))
#' as_gts(x)
#'
as_gts <- function(x, class = NULL, labels = list()) {
  x              <- tibble::as_tibble(drop_na_col(x))
  new_attributes <- list(gts = list(class = class, labels = labels))
  attributes(x)  <- c(attributes(x), new_attributes)
  class(x)       <- c("gts", class(x))
  x
}

#' @export
#'
as.data.frame.gts <- function(x, ...) {
  class(x) <- class(x)[!class(x) == "gts"]
  attr(x, "gts") <- NULL
  NextMethod()
}
