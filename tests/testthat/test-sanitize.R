test_that("sanitize", {
  expect <- "{ 'num' [ [ 1 2 3 4 5 6 7 8 9 10 ] '11' ] 'char' 'test' 'duration' 259200 s 'date' '2020-01-01T00:00:00Z' 'boolean' true 'warpscript' NOW NULL NULL }" # nolint
  res <- sanitize(
    list(
      num        = list(list(1:10), "11"),
      char       = "test",
      duration   = "3 days",
      date       = "2020-01-01",
      boolean    = TRUE,
      warpscript = "ws:NOW",
      null       = NULL
    )
  )
  expect_identical(res, expect)
})

test_that("sanitize to microseconds", {
  time <- lubridate::now(tzone = "UTC")
  date <- lubridate::today(tzone = "UTC")
  expect_equal(lubridate::as_datetime(as.numeric(sanitize(time, return = "microsecond")) / 1e6, tz = "UTC"), time)
  expect_equal(lubridate::as_date(as.numeric(sanitize(date, return = "microsecond")) / 8.64e10), date)
})

test_that("sanitize data.frame", {
  expect <- list(x = 1:10)
  res <- sanitize(data.frame(x = 1:10))
  expect_identical(res, expect)
})

test_that("sanitize with no arguments", {
  expect_equal(sanitize(), "")
})
