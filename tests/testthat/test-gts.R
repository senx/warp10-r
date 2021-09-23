test_that("possibility to create GTS from a dataframe", {
  df  <- data.frame(timestamp = 1:10, value = rnorm(10))
  gts <- as_gts(df, class = "test")
  expect_is(gts, "gts")
  expect_identical(as.data.frame(gts), df)
  expect_equal(
    object   = attr(gts, "gts"),
    expected = list(
      class      = "test",
      labels     = structure(list(), .Names = character(0)),
      attributes = structure(list(), .Names = character(0))
    )
  )
})

test_that("possibility to retrieve a GTS from a Warp10 database without values", {
  con <- wrp_connect()
  gts <- con %>%
    wrp_new_gts() %>%
    wrp_rename("test") %>%
    wrp_exec()
  expect_equal(
    object   = attr(gts, "gts"),
    expected = list(
      class      = "test",
      labels     = structure(list(), .Names = character(0)),
      attributes = structure(list(), .Names = character(0))
    )
  )
})

test_that("possibility to create and retrieve GTS with some values", {
  con <- wrp_connect()
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
  con  <- wrp_connect()
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
    wrp_get(0) %>%
    wrp_exec()
  expect_equal(nrow(gts), n)
})

test_that("unwrap work as expected", {
  df         <- data.frame(timestamp = 1:100, value = TRUE)
  gts        <- as_gts(df)
  gts_unwrap <- wrp_connect() %>%
    set_script("60V.5k.L.0N.5k..KV.N5GyA1.........0nNL7O4W..rXE6gwV....Lm.3G..") %>%
    wrp_unwrap() %>%
    wrp_exec()
  expect_equal(gts_unwrap, gts)
})


test_that("get work as expected", {
  res     <- list(NULL, 3, 42)
  get_res <- wrp_connect() %>%
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
    timestamp = c(1389139200000000, 1389139140000000, 1576674645000000),
    value     = c(21.5, 21.3, 42)
  )
  gts <- as_gts(
    x = df,
    class = "com.cityzendata.tutorial.sensors.temperature",
    labels = list(sensorId = "01"),
    attributes = list()
  )
  res <- list(
    c(1, 2, 3, 4, 42),
    c(1, 2, 3, 4),
    gts,
    gts[1:2, ]
  )
  res_clone <- wrp_connect() %>%
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
  res_relabel <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_clone() %>%
    wrp_relabel(foo = "bar", bar = "foo") %>%
    wrp_clone() %>%
    wrp_relabel("null" = NULL, star = "treck") %>%
    wrp_clone() %>%
    wrp_relabel("next" = "generation", "heckle" = "jeckle") %>%
    wrp_clone() %>%
    wrp_relabel("heckle" = NULL) %>%
    wrp_clone() %>%
    wrp_relabel("next" = "") %>%
    wrp_exec()
  expect_equal(res_relabel, res)
})

test_that("store work as expected", {
  res <- list()
  res_store <- wrp_connect() %>%
    set_script("42", add = "long") %>%
    wrp_store("foo") %>%
    wrp_exec()
  expect_equal(res, res_store)
})

test_that("drop work as expected", {
  res <- wrp_connect() %>%
    set_script("foo") %>%
    set_script("bar") %>%
    wrp_drop() %>%
    wrp_exec()

  expect_equal(res, "foo")
})

test_that("to selector work as expected", {
  res <- c("test%20name{label0=42,label1=foo}", "test%20name{label0=33,label1=foo}")
  res_to_selector <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_rename("test name") %>%
    wrp_relabel("label0" = "42", "label1" = "foo") %>%
    wrp_add_value(100, value = 10) %>%
    wrp_add_value(200, value = 9) %>%
    wrp_add_value(300, value = 8) %>%
    wrp_store("gts1") %>%
    set_script("$gts1", add = "gts") %>%
    wrp_clone() %>%
    wrp_relabel("label0" = "33") %>%
    wrp_store("gts2") %>%
    wrp_drop() %>%
    set_script("[ $gts1 $gts2 ]", add = "lgts") %>%
    wrp_to_selector() %>%
    wrp_exec()
  expect_equal(res_to_selector, res)
})


test_that("sort work as expected", {
  df  <- data.frame(timestamp = 10:1, value = as.integer(runif(10) * 10))
  res <- as_gts(df[order(df[["timestamp"]]), ])

  res_sort <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df, tick = timestamp) %>%
    wrp_sort() %>%
    wrp_exec()

  expect_equal(res_sort, res)
})

test_that("interpolate work as expected", {
  res <- as_gts(data.frame(timestamp = as.integer(seq(100, 500, by = 50)), value = seq(10, 6, by = -0.5)))
  df  <- res[res[["timestamp"]] %% 100 == 0, ]

  res_interpolate <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df, tick = "timestamp") %>%
    wrp_bucketize("mean", 500, 50, 0) %>%
    wrp_get(0) %>%
    wrp_interpolate() %>%
    wrp_sort() %>%
    wrp_exec()

  expect_equal(res_interpolate, res)
})

test_that("dup", {
  res <- list(123, 123)
  res_dup <- wrp_connect() %>%
    set_script(123, add = "numeric") %>%
    wrp_dup() %>%
    wrp_exec()

  expect_equal(res_dup, res)
})

test_that("remove_tick", {
  df             <- as_gts(data.frame(timestamp = 0:2, value = 0:2))
  res_removetick <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df, tick = "timestamp") %>%
    wrp_remove_tick(1) %>%
    wrp_exec()

  expect_equal(res_removetick, df[-2, ])
})

test_that("size", {
  df     <- data.frame(tick = seq(100, 1000, by = 100), value = 10:1)
  res_df <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df) %>%
    wrp_size() %>%
    wrp_exec()
  expect_equal(res_df, 10)

  res_map <- wrp_connect() %>%
    set_script("{ 'label0' '42' 'label1' 'foo' }", add = "map") %>%
    wrp_size() %>%
    wrp_exec()
  expect_equal(res_map, 2)

  res_list <- wrp_connect() %>%
    set_script("[ 'label0' '42' 'label1' 'foo' ]", add = "list") %>%
    wrp_size() %>%
    wrp_exec()
  expect_equal(res_list, 4)

  res_string <- wrp_connect() %>%
    set_script("one %25") %>%
    wrp_size() %>%
    wrp_exec()
  expect_equal(res_string, 5)
})

test_that("ticks", {
  df  <- data.frame(tick = 1:10, value = rnorm(10))
  res <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df) %>%
    wrp_ticks() %>%
    wrp_exec()
  expect_equal(res, unique(df[["tick"]]))
})

test_that("tick_list", {
  df  <- data.frame(tick = 1:10, value = rnorm(10))
  res <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df) %>%
    wrp_tick_list() %>%
    wrp_exec()
  expect_equal(res, df[["tick"]])
})

test_that("flatten", {
  x   <- c("a", "b", "c", "d", "e", "f", "g")
  res <- wrp_connect() %>%
    set_script("[ 'a' 'b' 'c' ]", add = "list") %>%
    set_script("[ 'd' 'e' [ 'f' 'g' ] ]", add = "list") %>%
    set_script("2 ->LIST", consume = c("list", "list"), add = "list") %>%
    wrp_flatten() %>%
    wrp_exec()
  expect_equal(res, x)
})

test_that("time_shift", {
  df     <- data.frame(timestamp = 1:100, value = 1:100)
  df_res <- as_gts(transform(df, timestamp = as.integer(timestamp + 10)))
  res    <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df, tick = "timestamp") %>%
    wrp_time_shift(10) %>%
    wrp_exec()
  expect_equal(df_res, res)
})

test_that("first tick", {
  df  <- data.frame(tick = c(seq(100, 500, by = 100), 100, 200), value = c(10:6, 10, 9))
  res <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df) %>%
    wrp_first_tick() %>%
    wrp_exec()
  expect_equal(res, min(df[["tick"]]))
})

test_that("last tick", {
  df  <- data.frame(tick = c(seq(100, 500, by = 100), 100, 200), value = c(10:6, 10, 9))
  res <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df) %>%
    wrp_last_tick() %>%
    wrp_exec()
  expect_equal(res, max(df[["tick"]]))
})

test_that("at tick", {
  df <- data.frame(tick = seq(100, 1000, by = 100), value = 10:1)
  df_res <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df) %>%
    wrp_at_tick(400) %>%
    wrp_exec()
  expect_equal(df_res, list(timestamp = 400, value = 7))
})

test_that("at index", {
  df <- data.frame(tick = seq(100, 1000, by = 100), value = 10:1)
  df_res <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_rename("test") %>%
    wrp_relabel("label0" = "42", "label1" = "foo") %>%
    wrp_add_value_df(df) %>%
    wrp_at_index(4) %>%
    wrp_exec()
  expect_equal(df_res, list(timestamp = 500, value = 6))
})

test_that("lr", {
  df <- data.frame(tick = 1:500, value = NA)
  set.seed(1234)
  df[["value"]][1] <- rnorm(1, sd = 3)
  for (j in seq_len(nrow(df))[-1]) {
    df[["value"]][j] <-  df[["value"]][j - 1] - rnorm(1, sd = 3) + 0.5
  }

  lr <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_rename("random") %>%
    wrp_add_value_df(df) %>%
    wrp_lr() %>%
    wrp_exec()

  r_lm <- lm(value ~ tick, df)$coefficients

  testthat::expect_equal(lr[[1]], r_lm[["tick"]])
  testthat::expect_equal(lr[[2]], r_lm[["(Intercept)"]])
})

test_that("parse_selector", {
  l <- list(
    c(sensorId =  "=01", sensortype = "~numeric.*"),
    "io.senx.tutorial.sensors.temperature"
  )
  res <- wrp_connect() %>%
    wrp_parse_selector("io.senx.tutorial.sensors.temperature{sensorId=01,sensortype~numeric.*}") %>%
    wrp_exec()
  testthat::expect_equal(res, l)
})

test_that("set_attributes", {
  x <- data.frame()
  expected <- list(
    as_gts(x, attributes = list(star = "treck", heckle = "Peter")),
    as_gts(x, attributes = list(heckle = "jeckle", star = "treck")),
    as_gts(x, attributes = list(star = "treck", "next" = "generation", heckle = "jeckle")),
    as_gts(x, attributes = list(star = "treck")),
    as_gts(x, attributes = list(bar = "foo", foo = "bar")),
    as_gts(x)
  )
  res <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_clone() %>%
    wrp_set_attributes(foo = "bar", bar = "foo") %>%
    wrp_clone() %>%
    wrp_set_attributes("NULL" = NULL, star = "treck") %>%
    wrp_clone() %>%
    wrp_set_attributes("next" = "generation", heckle = "jeckle") %>%
    wrp_clone() %>%
    wrp_set_attributes("next" = "") %>%
    wrp_clone() %>%
    wrp_set_attributes(heckle = "Peter") %>%
    wrp_exec()
  expect_equal(res, expected)
})

test_that("update, find, fetch, meta and delete works", {
  object <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_rename("gts_test") %>%
    wrp_add_value(1, 1) %>%
    wrp_update() %>%
    wrp_fetch(class = "gts_test") %>%
    wrp_set_attributes(foo = "bar") %>%
    wrp_meta() %>%
    wrp_find("gts_test") %>%
    wrp_clone() %>%
    wrp_to_selector() %>%
    wrp_get(0) %>%
    wrp_store("selector") %>%
    wrp_delete("ws:$selector", count = 1) %>%
    wrp_exec()

  expect_error(object, NA)
  expect_equal(object[[1]], 1)
  attr <- attr(object[[2]][[1]], "gts")
  class <- attr[["class"]]
  labels <- attr[["labels"]]
  attributes <- attr[["attributes"]]
  expect_equal(class, "gts_test")
  expect_equal(attributes, list(foo = "bar"))
})

test_that("attributes", {
  object <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_rename("test") %>%
    wrp_set_attributes(attr1 = "42", attr2 = "foo") %>%
    wrp_attributes() %>%
    wrp_exec()
  expected <- c(attr2 = "foo", attr1 = "42")
  expect_equal(object, expected)
})

test_that("map", {
  df_object   <- data.frame(tick = 0:4, value = 0:4)
  df_expected <- data.frame(timestamp = 0:4, value = c(0, rep(1e6, 4)))
  object <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df_object) %>%
    wrp_map("rate", 1) %>%
    wrp_exec()
  expected <- as_gts(df_expected)
  expect_equal(object[[1]], expected)
})

test_that("esdtest", {
  df <- tibble::tibble(tick = 1:1000, value = runif(1000) + runif(1000) +
                         runif(1000) + runif(1000) + runif(1000) + runif(1000)
                       - 3)
 df[368, "value"] <- -3.1
 df[422, "value"] <- 3.0001
 df[456, "value"] <- 9.8
 df[643, "value"] <- -200.9
 expected <- c(643, 456, 368, 422)
 object   <- wrp_connect() %>%
   wrp_new_gts() %>%
   wrp_add_value_df(df) %>%
   wrp_dedup() %>%
   wrp_esdtest(4, FALSE) %>%
   wrp_exec()
 expect_equal(object, expected)
})

test_that("unbucketize", {
  df <- data.frame(tick = 1:100, value = TRUE)
  object <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df) %>%
    wrp_bucketize("last", span = 10) %>%
    wrp_get(0) %>%
    wrp_unbucketize() %>%
    wrp_exec()
  expected <- as_gts(data.frame(timestamp = seq(100, 10, by = -10), value = TRUE))
  expect_equal(object, expected)
})

test_that("reduce", {
  df1        <- data.frame(tick = 1:10, value = 1)
  df2        <- data.frame(tick = 1:10, value = 2)
  expect_df1 <- as_gts(data.frame(timestamp = 1:10, value = 1), labels = list(type = "1"))
  expect_df2 <- as_gts(data.frame(timestamp = 1:10, value = 4), labels = list(type = "2"))
  expected   <- list(expect_df1, expect_df2)
  object     <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_rename("a") %>%
    wrp_relabel(type = "1") %>%
    wrp_add_value_df(df1) %>%
    wrp_new_gts() %>%
    wrp_rename("b") %>%
    wrp_relabel(type = "1") %>%
    wrp_add_value_df(df1) %>%
    wrp_new_gts() %>%
    wrp_rename("a") %>%
    wrp_relabel(type = "2") %>%
    wrp_add_value_df(df2) %>%
    wrp_new_gts() %>%
    wrp_rename("b") %>%
    wrp_relabel(type = "2") %>%
    wrp_add_value_df(df2) %>%
    set_script("4 ->LIST", consume = list("gts", "gts", "gts", "gts"), add = "lgts") %>%
    wrp_reduce("product", "type") %>%
    wrp_exec()
  expect_equal(object, expected)
})

test_that("reverse", {
  expected <- list("cat paws", c("drei", "zwei", "eins"))
  object   <- wrp_connect() %>%
    set_script(c("eins", "zwei", "drei")) %>%
    wrp_reverse() %>%
    set_script("swap tac") %>%
    wrp_reverse() %>%
    wrp_exec()
  expect_equal(object, expected)
})

test_that("values", {
  expected <- 10:1
  df <- data.frame(tick = c(100, 200, 300, 400, 500, 700, 800, 900, 1000, 1100), value = 10:1)
  object <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_add_value_df(df) %>%
    wrp_values() %>%
    wrp_exec()
  expect_equal(object, expected)
})

test_that("labels", {
  expected <- c(label0 = "42", label1 = "foo")
  object <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_relabel(label0 = "42", label1 = "foo") %>%
    wrp_labels() %>%
    wrp_exec()
  expect_equal(object, expected)
})

test_that("name", {
  expected <- "GTS1"
  object <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_rename("GTS1") %>%
    wrp_name() %>%
    wrp_exec()
  expect_equal(object, expected)
})

test_that("swap", {
  expected <- list("Second level", "Top", "Third level")
  object <- wrp_connect() %>%
    set_script("Third level") %>%
    set_script("Second level") %>%
    set_script("Top") %>%
    wrp_swap() %>%
    wrp_exec()
  expect_equal(object, expected)
})

test_that("to_list", {
  expected <- c("el1", "TRUE", "el4", "4")
  object <- wrp_connect() %>%
    set_script("el1") %>%
    set_script(TRUE) %>%
    set_script("el4") %>%
    set_script(4) %>%
    wrp_to_list(4) %>%
    wrp_exec()
  expect_equal(object, expected)
})

test_that("merge", {
  expected <- as_gts(
    data.frame(timestamp = c(1:5, 1, 7:10), value = c(1, 1, 1, 2, 2, 2, 3, 4, 4, 4)),
    class = "a",
    labels = list(label1 = "foo")
  )
  object <- wrp_connect() %>%
    wrp_new_gts() %>%
    wrp_rename("a") %>%
    wrp_relabel(label1 = "foo") %>%
    wrp_add_value_df(data.frame(tick = 1:5, value = c(1, 1, 1, 2, 2))) %>%
    wrp_new_gts() %>%
    wrp_rename("b") %>%
    wrp_relabel(label3 = "bar") %>%
    wrp_add_value_df(data.frame(tick = c(1, 7:10), value = c(2, 3, 4, 4, 4))) %>%
    wrp_to_list(2) %>%
    wrp_merge()  %>%
    wrp_exec()
  expect_equal(object, expected)
})

test_that("from_b64 and from_bytes", {
  expected <- "Warp 10 Geo Time Series™"
  object <- wrp_connect() %>%
    set_script("V2FycCAxMCBHZW8gVGltZSBTZXJpZXPihKI") %>%
    wrp_from_b64() %>%
    wrp_from_bytes() %>%
    wrp_exec()
  expect_equal(object, expected)
})

test_that("from_json", {
  expected <- list(
    list(menu = list(id = "file", value = "File")),
    list(menu = list(id = "file", value = "File"))
  )
  object <- wrp_connect() %>%
    set_script('[{"menu": {"id": "file","value": "File"}}]') %>%
    wrp_from_json() %>%
    wrp_get(0) %>%
    set_script('{"menu": {"id": "file","value": "File"}}') %>%
    wrp_from_json() %>%
    wrp_exec()
  expect_equal(expected, object)
})