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
    purrr::map2(stack, res, build_res, combine = combine, operator = operator)
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
  class(data) <- c(object, class(data))
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
#'
build_res.gts <- function(object, data, combine, operator) {
  new_data <- if (!is.data.frame(data[["v"]])) {
    build_gts_value(data[["v"]])
  } else {
    data[["v"]]
  }

  as_gts(new_data, class = data[["c"]], labels = data[["l"]], combine = combine, operator = operator)
}

#' @export
#'
build_res.lgts <- function(object, data, combine, operator) {
  if (length(data) == 0) return(data)
  metadata        <- list()
  n_values        <- purrr::map_int(data, ~ length(.x[["v"]]))
  classes         <- purrr::map_chr(data, "c")
  labels_df       <- purrr::map_dfr(data, "l")
  is_value        <- length(n_values) > 0L && all(n_values > 0L)
  new_data        <- if (is_value) {
    purrr::map(data, function(l) {
      build_gts_value(l[["v"]])
    })
  } else {
    labels_df
  }
  if (length(unique(classes)) > 1) {
    new_data <- if (is_value) {
      purrr::map2(new_data, classes, add_col, col_name = "class")
    } else {
      c(list(class = classes), new_data)
    }
  } else {
    metadata[["c"]] <- classes[[1]]
    if (!is_value) new_data[[classes[[1]]]] <- NULL
  }
  for (label in names(labels_df)) {
    if (length(unique(labels_df[[label]])) > 1) {
      if (is_value) new_data <- purrr::map2(new_data, labels_df[[label]], add_col, col_name = label)
    } else {
      metadata[["l"]] <- c(metadata[["l"]], stats::setNames(list(labels_df[[label]][1]), label))
      if (!is_value) new_data[[label]] <- NULL
    }
  }

  list_gts <- list(
    c = metadata[["c"]],
    l = metadata[["l"]],
    v = if (is_value) dplyr::bind_rows(new_data) else new_data
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

build_gts_value <- function(l) {
  n <- length(l)
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
