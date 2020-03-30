test_that("sanitize", {
  expect <- "{ 'num' [ 1 2 3 4 5 6 7 8 9 10 ] 'char' 'test' 'duration' 259200 s 'date' '2019-12-31T23:00:00Z' 'boolean' 'TRUE' 'warpscript' NOW }" # nolint
  res <- sanitize(
    list(
      num = list(1:10),
      char = "test",
      duration = "3 days",
      date = "2020-01-01",
      boolean = "true",
      warpscript = "ws:NOW",
      empty = NULL
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
