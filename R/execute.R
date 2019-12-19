#' Execute Warp Script
#'
#' Execute the Warp Script by sending it to the server.
#'
#' @inheritParams documentation
#' @param combine For LGTS, should all GTS be combined into one single GTS.
#' @param operator If `combine=TRUE` which function should be used to combine all the elements.
#'
#' @export
#'
wrp_exec <- function(wrp_con, combine = TRUE, operator = sum) {
  wrp_script <- get_script(wrp_con)
  endpoint   <- wrp_con$get_endpoint()
  stack      <- get_stack(wrp_con)
  raw_res    <- post_warpscript(warpscript = wrp_script, endpoint = endpoint)

  # Clear all scripts
  clear_script(wrp_con)

  # If an error occured and was not catched by R wrapper
  if (is.null(raw_res)) {
    return(cat(wrp_script))
  }

  res <- jsonlite::fromJSON(raw_res, simplifyVector = FALSE)

  if (length(stack) == 1) {
    build_res(stack[[1]], res[[1]], combine = combine, operator = operator)
  } else {
    purrr::map2(rev(stack), res, build_res, combine = combine, operator = operator)
  }
}

#' Build results
#'
#' Build results from parsed json file.
#'
#' @inheritParams wrp_exec
#' @param object A string corresponding to the object fetched from Warp10 database.
#' @param data A list resulting of a parsed json of all results.
#' @param ... Other arguments passed on to individual methods.
#'
#' @export
#'
build_res <- function(object, data, combine, operator) {
  if (!is.null(data)) {
    class(data) <- c(object, class(data))
  }
  UseMethod("build_res", data)
}

#' @export
#' @rdname build_res
#'
build_res.default <- function(object, data, ...) {
  return(data)
}

#' @export
#' @rdname build_res
build_res.list <- function(object, data, ...) {
  unlist(data)
}

#' @export
#' @rdname build_res
#'
build_res.gts <- function(object, data, combine, operator) {
  new_data <- build_gts_value(data)

  as_gts(new_data, class = data[["c"]], labels = unlist(data[["l"]]), combine = combine, operator = operator)
}

#' @export
#'
build_res.lgts <- function(object, data, combine, operator) {
  if (length(data) == 0) return(data)
  is_value <- all(purrr::map_int(.x = data, ~ length(.x[["v"]])) > 0)
  value    <- if (is_value) tibble::tibble(value = purrr::map(data, build_gts_value)) else NULL
  class    <- build_gts_class(data)
  label    <- build_gts_label(data)
  value    <- dplyr::bind_cols(class = class[["value"]], label = label[["value"]], value = value)
  if ("value" %in% names(value)) value <- tidyr::unnest(value, "value")

  list_gts <- list(
    c = class[["metadata"]],
    l = label[["metadata"]],
    v = value
  )

  build_res.gts(list_gts, object = "gts", combine = combine, operator = operator)
}

add_col <- function(df, y, col_name) {
  if (nrow(df) == 0) {
    df <- data.frame(V1 = NA, V2 = NA)
  }
  df[[col_name]] <- y
  df
}

drop_na_col <- function(df) {
  df[, colSums(!is.na(df)) > 0, drop = FALSE]
}

build_gts_value <- function(data) {
  l        <- data[["v"]]
  n        <- length(l)
  if (!is.null(names(l))) return(tibble::as_tibble(l))
  res <- tibble::tibble(
    timestamp = double(n),
    latitude  = NA,
    longitude = NA,
    elevation = NA,
    value     = NA
  )
  for (i in seq_along(l)) {
    ll <- l[[i]]
    res[i, "timestamp"] <- ll[[1]]
    if (length(ll) == 3) {
      res[i, "elevation"] <- ll[[2]]
    } else if (length(ll) == 5) {
      res[i, "latitude"]  <- ll[[3]]
      res[i, "longitude"] <- ll[[4]]
    }
    res[i, "value"] <- ll[[length(ll)]]
  }
  res <- drop_na_col(res)
  return(res)
}

build_gts_class <- function(data) {
  class    <- purrr::map_chr(data, "c")
  metadata <- NULL
  if (length(unique(class)) == 1) {
    metadata <- class
    class    <- NULL
  } else {
    class <- tibble::tibble(class = class)
  }
  return(list(value = class, metadata = metadata))
}

build_gts_label <- function(data) {
  label    <- purrr::map_dfr(data, "l")
  metadata <- NULL
  for (i in rev(seq_along(label))) {
    if (length(unique(label[[i]])) == 1) {
      metadata[names(label)[i]] <- unique(label[[i]])
      label[[i]]                <- NULL
    }
  }
  if (length(label) == 0) label <- NULL
  return(list(value = label, metadata = metadata))
}
