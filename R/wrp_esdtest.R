#' ESD test
#'
#' The ESDTEST function detects outliers in a GTS (or a LIST of GTS), by
#' applying a generalized extreme studentized deviate test.
#'
#' This test is done under the assumption that the GTS follows an approximately
#' normal distribution.
#'
#' A Grubbs’test is done for one candidate at a time. Then, the candidate is
#' removed from the set and another Grubbs’test is performed. This process is
#' iterated a given number of times. The detected outliers are the removed
#' values and the current candidate of the last successful test.
#'
#' A LIST of ticks (or a LIST of LIST of ticks), corresponding to the outliers,
#' is pushed back onto the stack.
#'
#' This function only applies to GTS of type DOUBLE.
#'
#' @inheritParams documentation
#' @param k Upperbound of the number of outliers to detect. If k is between 0
#' and 1, it is understood as the percentage of total value.
#' @param mad A flag indicating whether to use the mean (False), or the median
#' (True) to calculate the Z-score
#' @param alpha Optional significance level for the statistical test. Default
#' value is 0.05
#'
#' @references <https://www.warp10.io/doc/ESDTEST>
#'
#' @keywords gts, outlier
#'
#' @examples
#' df <- tibble::tibble(tick = 1:1000, value = runif(1000) + runif(1000) +
#'                      runif(1000) + runif(1000) + runif(1000) + runif(1000)
#'                      - 3)
#' df[368, "value"] <- -3.1
#' df[422, "value"] <- 3.0001
#' df[456, "value"] <- 9.8
#' df[643, "value"] <- -200.9
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_dedup() %>%
#'   wrp_esdtest(4, FALSE) %>%
#'   wrp_exec()
#'
#'
#' @export
#'
wrp_esdtest <- function(wrp_con, k, mad = TRUE, alpha = 0.05) {
  pre <- NULL
  if (k > 0 && k < 1) {
    pre <- "ws:DUP"
    k <- glue::glue("ws:{k} SWAP SIZE * TOLONG")
  }
  script <- paste(
    paste(sapply(purrr::compact(list(pre, k, mad, alpha)), sanitize), collapse = " "),
    "ESDTEST"
  )
  return_object <- list(
    gts = "list",
    lgts = "list"
  )
  add_stack(wrp_con, script, return_object)
}
