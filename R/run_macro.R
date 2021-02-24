#' Run Macro
#'
#' Run a macro either stored in warp10 repository (accessible via `@macro_name`), or a macro built with warpscript.
#'
#' @inheritParams documentation
#' @param macro Name of the macro
#' @param return_object Type of object returned by the macro
#' @param ... other parameters passed to macro
#' @param .eval Should be TRUE if macro built within warpscript
#'
#' @export
#'
run_macro <- function(wrp_con, macro, return_object = NULL, ..., .eval = FALSE) {
  script <- sanitize(...)
  if (.eval) {
    macro <- glue::glue("${macro} EVAL")
  }
  script <- paste(script, macro)
  add_stack(wrp_con, script, return_object)
}
