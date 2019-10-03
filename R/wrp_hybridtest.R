#' Hybrid Test
#'
#' The HYBRIDTEST function detects outliers in a GTS (or a LIST of GTS) which has a seasonal part.
#'
#' Like STLESDTEST, HYBRIDTEST performs an ESDTEST onto a GTS that have been relieved of its seasonal and trend part.
#' But unlike the mentioned test, STL and ESDTEST are performed piecewise.
#' Plus, the trend is approximated with the piecewise median instead of the trend part of the STL decomposition.
#'
#' A LIST of ticks (or a LIST of LIST of ticks), corresponding to the outliers, is pushed back onto the stack.
#'
#' This technique was first developped at Twitter.
#'
#' This function only applies to bucketized GTS of type DOUBLE.
#'
#' @inheritParams documentation
#' @param period Number of buckets that compose a period
#' @param piece Number of periods that compose a piece
#' @param k Upper-bound of the number of outliers to detect per piece
#' @param alpha Optional significance level for the statistical test. Default value is 0.05.
#' @param ... Optional parameters of the STL call. Not used for now.
#'
#' @export
#'
wrp_hybridtest <- function(wrp_con, period, piece, k, alpha = 0.05, ...) {
  script <- glue::glue("{period} {piece} {k} {alpha} HYBRIDTEST")
  wrp_con$set_script(script)
}
