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
