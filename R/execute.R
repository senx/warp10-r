#' Execute Warp Script
#'
#' Execute the Warp Script by sending it to the server.
#'
#' @inheritParams documentation
#'
#' @export
#'
wrp_exec <- function(wrp_con) {
  wrp_script <- get_script(wrp_con)
  endpoint   <- wrp_con$get_endpoint()
  stack      <- get_stack(wrp_con)
  raw_res    <- post_warpscript(warpscript = wrp_script, endpoint = endpoint)

  # If an error occured and was not catched by R wrapper
  if (is.null(raw_res)) {
    return(cat(wrp_script))
  }

  res <- jsonlite::fromJSON(raw_res, simplifyVector = FALSE)

  if (length(stack) == 1) {
    build_res(stack[[1]], res[[1]])
  } else {
    purrr::map2(stack, res, build_res)
  }
}

#' Build results
#'
#' Build results from parsed json file.
#'
#' @param object A string corresponding to the object fetched from Warp10 database.
#' @param data A list resulting of a parsed json of all results.
#'
#' @export
#'
build_res <- function(object, data) {
  class(data) <- c(object, class(data))
  UseMethod("build_res", data)
}

#' @export
#'
build_res.default <- function(object, data) {
  return(data)
}

#' @export
#'
build_res.gts <- function(object, data) {
  new_data                <- as.data.frame(data[["v"]], stringsAsFactors = TRUE)
  if (length(new_data) >= 2 && c("V1", "V2") %in% names(new_data)) {
    names(new_data)         <- c("timestamp", "value", names(new_data)[-c(1, 2)])
    new_data[["timestamp"]] <- lubridate::as_datetime(new_data[["timestamp"]] / 1e6)
  }
  new_data                <- tibble::as_tibble(drop_na_col(new_data))
  new_attributes          <- list(
    gts = list(
      class  = data[["c"]],
      labels = data[["l"]]
    )
  )
  attributes(new_data)    <- c(attributes(new_data), new_attributes)
  class(new_data)         <- c(object, class(new_data))
  new_data
}

#' @export
#'
build_res.lgts <- function(object, data) {
  metadata        <- list()
  n_values        <- purrr::map_int(data, ~ length(.x[["v"]]))
  classes         <- purrr::map_chr(data, "c")
  labels_df       <- purrr::map_dfr(data, "l")
  is_value        <- all(n_values > 0L)
  new_data        <- if (is_value) {
    purrr::map(data, function(l) {
      tibble::tibble(
        V1 = purrr::map_dbl(l[["v"]], 1),
        V2 = purrr::map_dbl(l[["v"]], 2)
      )
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

  build_res.gts(list_gts, object = "gts")
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
