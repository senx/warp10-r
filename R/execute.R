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
  raw_res    <- postWarpscript(warpscript = wrp_script, endpoint = endpoint)

  # If an error occured and was not catched by R wrapper
  if (is.null(raw_res)) {
    return(cat(wrp_script))
  }

  res <- jsonlite::fromJSON(raw_res, simplifyDataFrame = FALSE)

  if (length(stack) == 1) {
    build_res(stack[[1]], res[[1]])
  } else {
    purrr::map2(stack, res, build_res)
  }
}

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
  new_data                <- as.data.frame(data[["v"]])
  if (length(new_data) >= 2) {
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
  new_data        <- purrr::map(data, function(x) as.data.frame(x[["v"]]))
  classes         <- purrr::map_chr(data, "c")
  labels          <- purrr::map(data, "l")
  labels_df       <- dplyr::bind_rows(labels)
  other_names     <- NULL
  if (length(unique(classes)) > 1) {
    new_data        <- purrr::map2(new_data, classes, add_col, col_name = "class")
  } else {
    metadata[["c"]] <- classes[[1]]
  }
  for (label in names(labels_df)) {
    if (length(unique(labels_df[[label]])) > 1) {
      new_data        <- purrr::map2(new_data, labels_df[[label]], add_col, col_name = label)
    } else {
      metadata[["l"]] <- c(metadata[["l"]], setNames(list(labels_df[[label]][1]), label))
    }
  }

  list_gts <- list(
    c = metadata[["c"]],
    l = metadata[["l"]],
    v = dplyr::bind_rows(new_data)
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
