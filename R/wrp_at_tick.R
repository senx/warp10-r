#' At tick
#'
#' The ATTICK function consumes a Geo Time Series™ from the stack, looks at its tick-th tick
#' (the data point with a timestamp tick) and put on the stack a list with the timestamp,
#' longitude, latitude, elevation and value for that data point.
#'
#' If there is no data point with tick timestamp, it return a list with timestamp, longitude,
#' latitude and elevation at NaN and value at null.
#'
#' @inheritParams documentation
#' @param timestamp Timestamp at which to take the data.
#'
#' @seealso [wrp_at_index()]]
#'
#' @export
#'
#' @references <https://www.warp10.io/doc/ATTICK>
#'
#' @examples
#'
#' df <- data.frame(tick = seq(100, 1000, by = 100), value = 10:1)
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_at_tick(400) %>%
#'   wrp_exec()
wrp_at_tick <- function(wrp_con, timestamp) {
  add_stack(wrp_con, paste(sanitize(timestamp), "ATTICK"), list(gts = "data", lgts = "ldata"))
}
