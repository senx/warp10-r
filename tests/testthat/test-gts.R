test_that("possibility to create GTS from a dataframe", {
  df  <- data.frame(timestamp = 1:10, value = rnorm(10))
  gts <- as_gts(df, class = "test")
  expect_is(gts, "gts")
  expect_identical(as.data.frame(gts), df)
  expect_equal(attr(gts, "gts"), list(class = "test", labels = structure(list(), .Names = character(0))))
})

test_that("possibility to retrieve a GTS from a Warp10 database without values", {
  con <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec")
  gts <- con %>%
    wrp_new_gts() %>%
    wrp_rename("test") %>%
    wrp_exec()
  expect_equal(attr(gts, "gts"), list(class = "test", labels = structure(list(), .Names = character(0))))
})

test_that("possibility to create and retrieve GTS with some values", {
  con <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec")
  get_gts <- function(df) {
    con %>%
      wrp_new_gts() %>%
      wrp_add_value_df(df, tick = "timestamp") %>%
      wrp_exec()
  }
  df1 <- tibble::tibble(
    timestamp = seq.Date(lubridate::today() - lubridate::dweeks(20), lubridate::today(), by = "1 days"),
    value     = sample(c(TRUE, FALSE), replace = TRUE, size = 20 * 7 + 1)
  )
  df2 <- df1
  df2[["value"]] <- rnorm(20 * 7 + 1)
  gts1 <- get_gts(df1)
  gts2 <- get_gts(df2)
  expect_identical(gts1, as_gts(df1))
  expect_equal(as.data.frame(gts2), as.data.frame(as_gts(df2)))
})

test_that("bucketize works as expected", {
  con <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec")
  n   <- 4
  df <- tibble::tibble(
    timestamp = seq.Date(
      from = lubridate::today() - lubridate::dweeks(n) + lubridate::ddays(1),
      to   = lubridate::today(),
      by   = "1 days"
      ),
    value     = rnorm(n * 7)
  )
  gts <-  con %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df, tick = "timestamp") %>%
    wrp_bucketize("mean", span = "7 d") %>%
    wrp_exec()
  expect_equal(nrow(gts), n + 1)
})
