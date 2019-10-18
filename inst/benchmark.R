library(microbenchmark)

wrp_script <- " [ 'jA3Bc4jdo.wnulbuU2m22fM6MltAmPE6IhzniEbQS6X0TzuSDrm8DtGnLlAX6eWqii2dKSslAbyEFru.4JVd74WElSrgzB24Yh7q3wDTMbl6t2a.SWBH031RaZKaCRLCr_DTVM9Pz1nSEyYgIWoigrjsdyuoYtEH' '~.*' {  } NOW -5 ] FETCH \n"
data <- jsonlite::fromJSON(postWarpscript(wrp_script, endpoint = get_endpoint()), simplifyVector = FALSE)[[1]]

f1 <- function() purrr::map(data, function(l) as.data.frame(l[["v"]]))
f2 <- function() purrr::map(purrr::map(data, "v"), as.data.frame)
f3 <- function() purrr::map(data, function(l) purrr::map_dfr(l[["v"]], tibble::as_tibble, .name_repair = ~ c("V1", "V2")))

microbenchmark::microbenchmark(f1(), f2(), f3(), times = 5)


map_mat <- function (.x, .f, ..., .id = NULL)
{
  if (!is_installed("dplyr")) {
    abort("`map_df()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}
