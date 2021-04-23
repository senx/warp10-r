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
  if (length(lgts) == 0) {
    return(as_gts(data.frame()))
  }
  is_value  <- purrr::map_int(.x = lgts, ~ nrow(.x) > 0)
  value     <- if (any(is_value)) {
    if (!all(is_value)) {
      warning("Droping GTS with no values.", immediate. = TRUE)
    }
    tibble::tibble(value = lgts)
  } else {
    NULL
  }
  class     <- build_col_gts(lgts, col = "class")
  label     <- build_col_gts(lgts, col = "labels")
  attribute <- build_col_gts(lgts, col = "attributes")
  df        <- tibble::tibble(id = as.character(seq_along(lgts)))
  for (col in list(class, label, attribute)) {
    if (!is.null(col[["value"]])) {
      df <- dplyr::left_join(df, col[["value"]], by = "id")
    }
  }
  df[["id"]] <- NULL
  if (ncol(df) == 0) {
    df <- NULL
  }
  value      <- dplyr::bind_cols(df, value = value)
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
  l <- data[["v"]]
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

build_col_gts <- function(lgts, col) {
  x        <- purrr::map(.x = lgts, ~ attr(.x, "gts")[[col]])
  metadata <- NULL
  if (length(unique(x)) == 1) {
    metadata <- unique(x)
    x        <- NULL
  } else {
    if (col == "class") {
      x <- tibble::tibble(id = as.character(seq_along(lgts)), class = unlist(x))
    } else {
      x <- dplyr::bind_rows(x, .id = "id")
    }
  }
  if (length(x) == 0) x <- NULL
  return(list(value = x, metadata = metadata))
}
