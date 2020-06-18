#' @importFrom tibble type_sum
#' @export
#'
type_sum.gts <- function(x) {
  "GTS"
}

#' @importFrom tibble type_sum
#' @export
#'
type_sum.lgts <- function(x) {
  "LGTS"
}

#' @importFrom tibble tbl_sum
#' @export
#'
tbl_sum.gts <- function(x) {
  attributes <- attr(x, "gts")
  class      <- c("class" = attributes[["class"]])
  if (!is.null(class)) class <- c("class" = paste0(class, "\n"))
  labels     <- attributes[["labels"]]
  attributes <- attributes[["attributes"]]
  return(c("A GTS object" = glue::glue("{nrow(x)} x {ncol(x)}"), class, labels, attributes))
}
