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
  con  <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec")
  n    <- 4
  date <- lubridate::ymd("2019-12-19")
  df   <- tibble::tibble(
    timestamp = seq.Date(
      from = date - lubridate::dweeks(n) + lubridate::ddays(1),
      to   = date,
      by   = "1 days"
    ),
    value     = rnorm(n * 7)
  )
  gts <-  con %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df, tick = "timestamp") %>%
    wrp_bucketize("mean", span = "7 d") %>%
    wrp_exec()
  expect_equal(nrow(gts), n)
})

test_that("find and fetch work as expected", {
  if (length(get_token()) > 0) {
    con <- wrp_connect()
    script <- wrp_find(con)
    expect_error(df <- wrp_exec(script), NA)
    labels <- as.list(df[2, !names(df) %in% "class"])
    labels <- labels[!purrr::map_lgl(labels, is.na)]
    script <- wrp_fetch(con, class = df$class[2], labels = labels)
    expect_error(wrp_exec(script), NA)
  }
})

test_that("unwrap work as expected", {
  df         <- data.frame(timestamp = as.numeric(1:100), value = TRUE)
  gts        <- as_gts(df)
  gts_unwrap <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    set_script("60V.5k.L.0N.5k..KV.N5GyA1.........0nNL7O4W..rXE6gwV....Lm.3G..") %>%
    wrp_unwrap() %>%
    wrp_exec()
  expect_equal(gts_unwrap, gts)
})


test_that("get work as expected", {
  res     <- list(NULL, 3, 42)
  get_res <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    set_script("{ 'foo' 42 'bar' true }", add = "map") %>%
    wrp_get("foo") %>%
    set_script("[ 3 12 15 ]", add = "map") %>%
    wrp_get(0) %>%
    set_script("{ 'foo' 42 'bar' true }", add = "map") %>%
    wrp_get(33) %>%
    wrp_exec()
  expect_equal(get_res, res)
})

test_that("clone work as expected", {
  df  <- data.frame(
    timestamp = c(1389139140000000, 1389139200000000, 1576674645000000),
    value     = c(21.3, 21.5, 42)
  )
  gts <- as_gts(df, class = "com.cityzendata.tutorial.sensors.temperature", labels = list(sensorId = "01"))
  res <- list(
    c(1, 2, 3, 4, 42),
    c(1, 2, 3, 4),
    gts,
    gts[1:2, ]
  )
  res_clone <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    set_script("[ '60VgNqxhAaCdS6_uOLtZNMGWAbGpS5xmPL4gAbC_QbCjRbBiS5KhR5KmNMGpRaJQ.NV7RqKiRqxmHLF1B23L.0N.5k..KV.N6bF.0DxeA7x..3.pV.......4EyQb2_.CJnBnBnBnGN33V.' '60VgNqxhAaCdS6_uOLtZNMGWAbGpS5xmPL4gAbC_QbCjRbBiS5KhR5KmNMGpRaJQ.NV7RqKiRqxmHLF1B27L.0N.5k..KV.N6rF.0Dxe1Bju.3.ltaOaOaOa472Jp9g0F26BnBnBnBoL007.' ]", add = "list") %>% # nolint
    wrp_unwrap() %>%
    wrp_get(0) %>%
    wrp_clone() %>%
    wrp_add_value(tick = 1576674645000000, value = 42) %>%
    set_script("[ 1 2 3 4 ]", add = "list") %>%
    wrp_clone() %>%
    set_script("42 +!", consume = "list", add = "list") %>%
    wrp_exec()
  expect_equal(res_clone, res)
})

test_that("relabel work as expected", {
  df <- data.frame()
  res <- list(
    as_gts(df, labels = c(star = "treck")),
    as_gts(df, labels = c("next" = "generation", star = "treck")),
    as_gts(df, labels = c(star = "treck", "next" = "generation", heckle = "jeckle")),
    as_gts(df, labels = c(star = "treck")),
    as_gts(df, labels = c(bar = "foo", foo = "bar")),
    as_gts(df)
  )
  res_relabel <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    wrp_new_gts() %>%
    wrp_clone() %>%
    wrp_relabel(c("foo", "bar", "bar", "foo")) %>%
    wrp_clone() %>%
    wrp_relabel(list(NULL, NULL, "star", "treck")) %>%
    wrp_clone() %>%
    wrp_relabel(c("next", "generation", "heckle", "jeckle")) %>%
    wrp_clone() %>%
    wrp_relabel(list("heckle", NULL)) %>%
    wrp_clone() %>%
    wrp_relabel(c("next", "")) %>%
    wrp_exec()
  expect_equal(res_relabel, res)
})

test_that("store work as expected", {
  res <- list()
  res_store <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    set_script("42", add = "long") %>%
    wrp_store("foo") %>%
    wrp_exec()
  expect_equal(res, res_store)
})

test_that("drop work as expected", {
  res <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    set_script("foo") %>%
    set_script("bar") %>%
    wrp_drop() %>%
    wrp_exec()

  expect_equal(res, "foo")
})

test_that("to selector work as expected", {
  res <- c("test%20name{label0=42,label1=foo}", "test%20name{label0=33,label1=foo}")
  res_to_selector <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    wrp_new_gts() %>%
    wrp_rename("test name") %>%
    wrp_relabel(c("label0", 42, "label1", "foo")) %>%
    wrp_add_value(100, value = 10) %>%
    wrp_add_value(200, value = 9) %>%
    wrp_add_value(300, value = 8) %>%
    wrp_store("gts1") %>%
    set_script("$gts1", add = "gts") %>%
    wrp_clone() %>%
    wrp_relabel(c("label0", "33")) %>%
    wrp_store("gts2") %>%
    wrp_drop() %>%
    set_script("[ $gts1 $gts2 ]", add = "gtslist") %>%
    wrp_to_selector() %>%
    wrp_exec()
  expect_equal(res_to_selector, res)
})


test_that("sort work as expected", {
  df  <- data.frame(timestamp = as.numeric(10:1), value = as.integer(runif(10) * 10))
  res <- df[order(df[["timestamp"]]), ]

  res_sort <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df, tick = timestamp) %>%
    wrp_sort() %>%
    wrp_exec()

  expect_equal(res_sort, res)
})

test_that("interpolate work as expected", {
  res <- data.frame(timestamp = seq(100, 500, by = 50), value = seq(10, 6, by = -0.5))
  df  <- res[res[["timestamp"]] %% 100 == 0, ]

  res_interpolate <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec") %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df, tick = "timestamp") %>%
    wrp_bucketize("mean", 500, 50, 0) %>%
    wrp_interpolate() %>%
    wrp_sort() %>%
    wrp_exec()

  expect_equal(res_interpolate, res)
})
