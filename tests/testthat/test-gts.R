test_that("as_gts works", {
  df  <- data.frame(timestamp = 1:10, value = rnorm(10))
  gts <- as_gts(df, class = "test")
  expect_is(gts, "gts")
  expect_identical(as.data.frame(gts), df)
  expect_equal(attr(gts, "gts"), list(class = "test", labels = list()))
  gts2 <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    clear_script() %>%
    wrp_new_gts() %>%
    wrp_rename("test") %>%
    wrp_exec()
  expect_equal(attr(gts2, "gts")[["class"]], "test")
})
