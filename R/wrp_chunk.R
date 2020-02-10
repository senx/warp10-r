#' Chunk
#'
#' The CHUNK is used to split a Geo Time Series™ into partial GTS.
#' The split operation is controlled by multiple parameters.
#' The series will be splitted in chunks, each chunks will form a new GTS,
#' with a label value corresponding to its first tick.
#'
#' @inheritParams documentation
#' @param last The end timestamp of the most recent split to consider.
#' @param width The width in time units of each chunk.
#' @param overlap An overlap duration (in time units) between chunks,
#'   this is useful when chunking a GTS to apply an algorithm which operates on a sliding window.
#' @param count The number of chunks to keep (starting from the last chunk).
#' @param label The label name of the label added by the split operation
#'   (with a value corresponding to the first tick of the chunk).
#' @param keep_empty If true empty chunks are kept.
#'
#' @examples
#' df <- data.frame(tick = seq(100, 1100, by = 100)[-6], value = 10:1)
#'
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_rename("test") %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_chunk(
#'     last       = 1000,
#'     width      = 500,
#'     overlap    = 0,
#'     count      = 0,
#'     label      = ".chunkid",
#'     keep_empty = FALSE
#'   ) %>%
#'   wrp_exec()
#' @export
#'
wrp_chunk <- function(wrp_con, last, width, overlap, count, label, keep_empty) {
  last       <- parse_timestamp(last)
  width      <- parse_time_unit(width)
  overlap    <- parse_time_unit(overlap)
  label      <- sanitize(label)
  keep_empty <- parse_boolean(keep_empty)

  script         <- paste(last, width, overlap, count, label, keep_empty, "CHUNK")
  return_object  <- list("gts" = "lgts", "lgts" = "lgts", encoder = "lenconder", lenconder = "lencoder")

  add_stack(wrp_con, script, return_object)
}
