#' Merge
#'
#' Merge several Geo Time Series™ together.
#'
#' The MERGE function expects a LIST of Geo Time Series™ on the top of the stack.
#' It will consume these GTS and push onto the stack a single GTS instance with all measurements found in the GTS
#' instances present in the LIST.
#' The name and labels of the resulting GTS instance are those of the first one of the LIST.
#'
# " MERGE do not override values on the same timestamp, they are appenned.
#'
#' @inheritParams documentation
#'
#' @keywords gts
#'
#' @export
#'
#' @examples
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_rename("a") %>%
#'   wrp_relabel(label1 = "foo") %>%
#'   wrp_add_value_df(data.frame(tick = 1:5, value = c(1, 1, 1, 2, 2))) %>%
#'   wrp_new_gts() %>% wrp_rename("b") %>%
#'   wrp_relabel(label3 = "bar") %>%
#'   wrp_add_value_df(data.frame(tick = c(1, 7:10), value = c(2, 3, 4, 4, 4))) %>%
#'   wrp_merge() %>%
#'   wrp_exec()
wrp_merge <- function(wrp_con) {
  return_object <- list(
    lgts = "gts"
  )
  add_stack(wrp_con, "MERGE", return_object)
}
