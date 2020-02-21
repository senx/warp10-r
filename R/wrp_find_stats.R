#' Find stats
#'
#' The FINDSTATS function computes statistics on matching Geo Time Series™.
#' The estimations are based on the use of HyperLogLogPlus estimators.
#'
#' @return The function returns a map containing information about the matching Geo Time Series.
#' The fields of the map are the following:
#'
#' - `gts.estimate`	Estimation of the number of matching Geo Time Series™
#'
#' - `classes.estimate`	Estimation of the number of distinct class names
#'
#' - `labelnames.estimate`	Estimation of the number of distinct label names
#'
#' - `labelvalues.estimate`	Estimation of the number of distinct label values
#'
#' - `per.class.estimate`	If the number of matching classes is below the
#'
#' `directory.stats.class.maxcardinality`, this key will be associated with a map containing per
#' class estimate of number of GTS
#'
#' - `per.label.value.estimate`	If the number of label names in the matching GTS is below
#'
#' `directory.stats.labels.maxcardinality`, this key will be associated with a map containing
#' an estimation of distinct label values per label name
#'
#' - `error.rate`	This is the error rate of the estimators used for computing the estimations
#'
#' - `partial.results`	When accessing a sharded Directory, this will be set to true if only
#' partial results were collected
#'
#' @inheritParams documentation
#'
#' @seealso [wrp_find()], [wrp_find_sets()]
#'
#' @references <https://www.warp10.io/doc/FINDSTATS>
#'
#' @keywords gts
#'
#' @export
#'
wrp_find_stats <- function(wrp_con, class = "~.*", labels = NULL) {
  assert_token(wrp_con$get_token())
  labels <- labels_to_string(labels)
  script <- glue::glue(
    "[ $token '{class}' {{ {labels} }} ] FINDSTATS"
  )
  add_stack(wrp_con, script, "map")
}
