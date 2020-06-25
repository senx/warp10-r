#' Bind LGTS
#'
#' Bind LGTS into a single GTS
#'
#' @param lgts LGTS dataframe
#' @param combine If TRUE, combine multiple time series into one by grouping dates.
#' @inheritParams dplyr::summarise_at
#'
#' @export
#'
bind_lgts <- function(lgts, combine = FALSE, .funs = "first") {
  is_value  <- all(purrr::map_int(.x = lgts, ~ nrow(.x) > 0))
  value     <- if (is_value) tibble::tibble(value = purrr::map(.x = lgts, ~ .x)) else NULL
  class     <- build_gts_class(lgts)
  label     <- build_gts_label(lgts)
  attribute <- build_gts_attributes(lgts)
  value     <- dplyr::bind_cols(
    class     = class[["value"]],
    label     = label[["value"]],
    value     = value,
    attribute = attribute[["value"]]
  )
  if ("value" %in% names(value)) value <- tidyr::unnest(value, "value")

  gts <- as_gts(
    x          = value,
    class      = class[["metadata"]],
    labels     = label[["metadata"]],
    attributes = attribute[["metadata"]]
  )

  if (combine && nrow(gts) > 0 && "timestamp" %in% names(gts)) {
    .funs <- eval(parse(text = .funs))
    gts <- dplyr::summarise_at(dplyr::group_by_at(gts, "timestamp"), "value", .funs)
  }

  return(gts)
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

build_gts_class <- function(lgts) {
  class    <- purrr::map_chr(.x = lgts, ~ attr(.x, "gts")[["class"]])
  metadata <- NULL
  if (length(unique(class)) == 1) {
    metadata <- unique(class)
    class    <- NULL
  } else {
    class <- tibble::tibble(class = class)
  }
  return(list(value = class, metadata = metadata))
}

build_gts_label <- function(lgts) {
  label    <- purrr::map(.x = lgts, ~ attr(.x, "gts")[["labels"]])
  metadata <- NULL
  if (length(unique(label)) == 1) {
    metadata <- unique(label)
    label    <- NULL
  } else {
    label <- dplyr::bind_rows(label)
  }
  if (length(label) == 0) label <- NULL
  return(list(value = label, metadata = metadata))
}

build_gts_attributes <- function(lgts) {
  attribute <- purrr::map(.x = lgts, ~ attr(.x, "gts")[["attributes"]])
  metadata  <- NULL
  if (length(unique(attribute)) == 1) {
    if (length(attribute[[1]]) > 0) {
      metadata  <- unique(attribute)
    }
    attribute <- NULL
  } else {
    attribute <- tibble::tibble(label = attribute)
  }
  if (length(attribute) == 0) attribute <- NULL
  return(list(value = attribute, metadata = metadata))
}
