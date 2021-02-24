#' Execute Warp Script
#'
#' Execute the Warp Script by sending it to the server.
#'
#' @inheritParams documentation
#'
#' @export
#'
wrp_exec <- function(wrp_con) {
  wrp_script <- wrp_con$get_script()
  endpoint   <- wrp_con$get_endpoint()
  stack      <- get_stack(wrp_con)
  raw_res    <- post_warpscript(warpscript = wrp_script, endpoint = endpoint)
  tz         <- wrp_con$get_tz()

  # Clear all scripts
  clear_script(wrp_con)

  # If an error occured and was not catched by R wrapper
  if (is.null(raw_res)) {
    return(cat(wrp_script))
  }

  res <- jsonlite::fromJSON(raw_res, simplifyVector = FALSE)
  if (length(res) != length(stack)) {
    msg <- glue::glue("Number of elements declared in the stack ({length(stack)}) does not match the number of fetched data ({length(res)}).")
    stop(msg)
  }
  res <- purrr::map2(rev(stack), res, function(class, x) {
    if (!is.null(x)) {
      class(x) <- c(class, class(x))
    }
    return(x)
  })

  if (length(stack) == 1) {
    build_res(res[[1]], tz = tz)
  } else {
    purrr::map(res, build_res, tz = tz)
  }
}

#' Build results
#'
#' Build results from parsed json file.
#'
#' @param data A list resulting of a parsed json of all results.
#' @param tz Time Zone
#' @param ... Other paramters passed into methods
#'
#' @export
#'
build_res <- function(data, tz, ...) {
  UseMethod("build_res")
}

#' @export
#' @rdname build_res
#'
build_res.data <- function(data, tz, ...) {
  purrr::compact(purrr::set_names(data, c("timestamp", "latitude", "longitude", "elevation", "value")))
}

#' @export
#' @rdname build_res
#'
build_res.ldata <- function(data, tz, ...) {
  as_gts(purrr::map_dfr(data, function(l) {
    as.data.frame(
      purrr::compact(
        purrr::set_names(l, nm = c("timestamp", "latitude", "longitude", "elevation", "value"))
      )
    )
  }), tz = tz)
}

#' @export
#' @rdname build_res
#'
build_res.default <- function(data, tz, ...) {
  return(unclass(data))
}

#' @export
#' @rdname build_res
build_res.map <- function(data, tz, ...) {
  build_res.list(data)
}

#' @export
#' @rdname build_res
build_res.lastactivity <- function(data, tz, ...) {
  lubridate::as_datetime(data / 1e6, tz = tz)
}

#' @export
#' @rdname build_res
build_res.list <- function(data, tz, ...) {
  if (all(sapply(data, length) == 1)) {
    unclass(unlist(data))
  } else {
    unclass(data)
  }
}

#' @export
#' @rdname build_res
#'
build_res.gts <- function(data, tz, ...) {
  new_data <- build_gts_value(data)

  as_gts(new_data, class = data[["c"]], labels = unlist(data[["l"]]), attributes = unlist(data[["a"]]), tz = tz)
}

#' @export
#' @rdname build_res
#'
build_res.lgts <- function(data, tz, ...) {
  if (length(data) == 0) return(data)
  purrr::map(data, build_res.gts, tz = tz)
}
