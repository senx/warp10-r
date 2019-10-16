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

  res <- jsonlite::fromJSON(raw_res)

  if (length(stack) > 1) {
    stack <- "lgts"
  }
  build_res(stack, res)
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
build_res.gts <- function(object, data, other_names = NULL) {
  new_data                <- as.data.frame(data[["v"]][[1]])
  if (length(new_data) >= 2) {
    names(new_data)         <- c("timestamp", "value", other_names)
    new_data[["timestamp"]] <- lubridate::as_datetime(new_data[["timestamp"]] / 1e6)
  }
  new_data                <- tibble::as_tibble(drop_na_col(new_data))
  new_attributes          <- list(
    gts = list(
      class  = data[["c"]],
      labels = as.list(data[["l"]])
    )
  )
  attributes(new_data)    <- c(attributes(new_data), new_attributes)
  class(new_data)         <- c(object, class(new_data))
  new_data
}

#' @export
#'
build_res.lgts <- function(object, data) {
  data[["v"]] <- purrr::map(data[["v"]], as.data.frame)
  classes     <- data[["c"]]
  other_names <- NULL
  if (length(unique(classes)) > 1) {
    data[["v"]] <- purrr::map2(data[["v"]], as.character(data[["c"]]), add_col)
    data[["c"]] <- NULL
    other_names <- c(other_names, "class")
  }
  labels     <- data[["l"]]
  len_labels <- purrr::map_int(labels, function(x) length(unique(x)))
  for (label in names(len_labels)[len_labels > 1]) {
    data[["v"]]          <- purrr::map2(data[["v"]], as.character(data[["l"]][[label]]), add_col)
    data[["l"]][[label]] <- NULL
    other_names <- c(other_names, label)
  }
  new_data <- data %>%
    split(seq_len(nrow(.))) %>%
    purrr::map_dfr(build_res.gts, object = "gts", other_names = other_names) %>%
    tibble::as_tibble()
  new_attributes          <- list(
    gts = list(
      class  = data[["c"]],
      labels = purrr::map(data[["l"]], unique)
    )
  )
  attributes(new_data)    <- c(attributes(new_data), new_attributes)
  class(new_data)         <- c("gts", class(new_data))
  new_data
}

add_col <- function(x, y) {
  if (nrow(x) == 0) {
    data.frame(V1 = NA, V2 = NA, V3 = y, stringsAsFactors = FALSE)
  } else {
    cbind(x, y, stringsAsFactors = FALSE)
  }
}

drop_na_col <- function(df) {
  df[, colSums(!is.na(df)) > 0, drop = FALSE]
}
