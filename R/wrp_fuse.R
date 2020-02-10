#' Fuse
#'
#' Fuses Geo Time Series™ chunks.
#' The fusion process attempts to keep the bucketization parameters if all chunks are bucketized
#' with compatible bucketspan and lastbucket values.
#'
#' All chunks must be of the same type.
#'
#' The fused GTS will have the common class name of the chunks or no class name if some chunks have
#' a different class name.
#' The labels will be those common to all chunks, so when fusing chunks created with CHUNK,
#' the chunk id will be dropped.
#'
#' @inheritParams documentation
#'
#' @examples
#' df <- data.frame(tick = 1:100, value = rnorm(100))
#'
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_relabel(c("foo", "bar")) %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_chunk(100, 10, 0, 10, "chunkid", FALSE) %>%
#'   wrp_dup() %>%
#'   wrp_fuse() %>%
#'   wrp_exec(combine = FALSE)
#' @export
#'
wrp_fuse <- function(wrp_con) {
  add_stack(wrp_con, script = "FUSE", return_object = list(lgts = "gts"))
}
