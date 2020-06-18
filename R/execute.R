#' Execute Warp Script
#'
#' Execute the Warp Script by sending it to the server.
#'
#' @inheritParams documentation
#' @inheritParams as_gts
#'
#' @export
#'
wrp_exec <- function(wrp_con, combine = TRUE, .funs = "first") {
  wrp_script <- wrp_con$get_script()
  endpoint   <- wrp_con$get_endpoint()
  stack      <- get_stack(wrp_con)
  raw_res    <- post_warpscript(warpscript = wrp_script, endpoint = endpoint)

  # Clear all scripts
  clear_script(wrp_con)

  # If an error occured and was not catched by R wrapper
  if (is.null(raw_res)) {
    return(cat(wrp_script))
  }

  .funs <- eval(parse(text = .funs))
  res <- jsonlite::fromJSON(raw_res, simplifyVector = FALSE)

  if (length(stack) == 1) {
    build_res(stack[[1]], res[[1]], combine = combine, .funs = .funs)
  } else {
    purrr::map2(rev(stack), res, build_res, combine = combine, .funs = .funs)
  }
}

#' Build results
#'
#' Build results from parsed json file.
#'
#' @inheritParams as_gts
#' @param object A string corresponding to the object fetched from Warp10 database.
#' @param data A list resulting of a parsed json of all results.
#' @param ... Other arguments passed on to individual methods.
#'
#' @export
#'
build_res <- function(object, data, combine, .funs) {
  if (!is.null(data)) {
    class(data) <- c(object, class(data))
  }
  UseMethod("build_res", data)
}

#' @export
#' @rdname build_res
#'
build_res.data <- function(object, data, ...) {
  purrr::compact(purrr::set_names(data, c("timestamp", "latitude", "longitude", "elevation", "value")))
}

#' @export
#' @rdname build_res
#'
build_res.ldata <- function(object, data, ...) {
  as_gts(purrr::map_dfr(data, function(l) {
    as.data.frame(
      purrr::compact(
        purrr::set_names(l, nm = c("timestamp", "latitude", "longitude", "elevation", "value"))
      )
    )
  }))
}

#' @export
#' @rdname build_res
#'
build_res.default <- function(object, data, ...) {
  return(data)
}

#' @export
#' @rdname build_res
build_res.map <- function(object, data, ...) {
  if (all(sapply(data, length) == 1)) {
    unlist(data)
  } else {
    data
  }
}

#' @export
#' @rdname build_res
build_res.lastactivity <- function(object, data, ...) {
  lubridate::as_datetime(data / 1e6)
}

#' @export
#' @rdname build_res
build_res.list <- function(object, data, ...) {
  if (all(sapply(data, length) == 1)) {
    unlist(data)
  } else {
    data
  }
}

#' @export
#' @rdname build_res
#'
build_res.gts <- function(object, data, combine, .funs) {
  new_data <- build_gts_value(data)

  as_gts(new_data, class = data[["c"]], labels = unlist(data[["l"]]), attributes = unlist(data[["a"]]), combine = combine, .funs = .funs)
}

#' @export
#'
build_res.lgts <- function(object, data, combine, .funs) {
  if (length(data) == 0) return(data)
  is_value  <- all(purrr::map_int(.x = data, ~ length(.x[["v"]])) > 0)
  value     <- if (is_value) tibble::tibble(value = purrr::map(data, build_gts_value)) else NULL
  class     <- build_gts_class(data)
  label     <- build_gts_label(data)
  attribute <- build_gts_attributes(data)
  value     <- dplyr::bind_cols(
    class     = class[["value"]],
    label     = label[["value"]],
    value     = value,
    attribute = attribute[["value"]]
  )
  if ("value" %in% names(value)) value <- tidyr::unnest(value, "value")

  list_gts <- list(
    c = class[["metadata"]],
    l = label[["metadata"]],
    a = attribute[["metadata"]],
    v = value
  )

  build_res.gts(list_gts, object = "gts", combine = combine, .funs = .funs)
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
  if (!is.null(names(l)) || length(l) == 0) return(tibble::as_tibble(l))
  n <- length(l[[1]])
  if (n == 2) {
    colnames <- c("timestamp", "value")
  } else if (n == 3) {
    colnames <- c("timestamp", "elevation", "value")
  } else if (n == 5) {
    colnames <- c("timestamp", "latitude", "longitude", "elevation", "value")
  } else {
    stop("Something went wrong when building the GTS.")
  }
  tibble::as_tibble(
    stats::setNames(
      object = lapply(seq_len(n), function(i) sapply(l, `[[`, i)),
      nm = colnames
    )
  )
}

build_gts_class <- function(data) {
  class    <- purrr::map_chr(data, "c")
  metadata <- NULL
  if (length(unique(class)) == 1) {
    metadata <- unique(class)
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

build_gts_attributes <- function(data) {
  attributes <- purrr::map_dfr(data, "a")
  metadata   <- NULL
  for (i in rev(seq_along(attributes))) {
    if (length(unique(attributes[[i]])) == 1) {
      metadata[names(attributes)[i]] <- unique(attributes[[i]])
      attributes[[i]]                <- NULL
    }
  }
  if (length(attributes) == 0) attributes <- NULL
  return(list(value = attributes, metadata = metadata))
}
