add_stack <- function(wrp_con, script, return_object) {
  if (is.null(names(return_object))) {
    last_object <- NULL
    new_object  <- as.character(return_object)
  } else {
    stack       <- get_stack(wrp_con)
    last_object <- stack[[length(stack)]]
    if (!last_object %in% names(return_object) && !"any" %in% names(return_object)) {
      fun_call    <- sys.call(sys.nframe() - 1)[[1]] # nolint
      error_msg   <- glue::glue("`{fun_call}` can't consume {last_object}.")
      stop(error_msg)
    }
    new_object  <- return_object[[last_object]]
    if (is.null(new_object) && "any" %in% names(return_object)) {
      new_object <- rep(last_object, length(return_object[["any"]]))
    }
  }
  set_script(wrp_con, script = script, consume = last_object, add = new_object)
}
