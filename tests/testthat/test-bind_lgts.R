test_that("Bind LGTS with values", {
  expect_error(regexp = NA, {
    wrp_connect() %>%

      wrp_new_gts() %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_relabel(foo = "bar") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_set_attributes(attr = "test") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_relabel(foo = "bar") %>%
      wrp_set_attributes(attr = "test") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_set_attributes(attr = "test") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_relabel(foo = "bar") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_relabel(foo = "bar") %>%
      wrp_set_attributes(attr = "test") %>%
      wrp_add_value(0, 0) %>%

      wrp_exec() %>%
      bind_lgts()

  })
})

test_that("Bind LGTS with no values", {
  expect_error(regexp = NA, {
    wrp_connect() %>%
      wrp_new_gts() %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%

      wrp_new_gts() %>%
      wrp_relabel(foo = "bar") %>%

      wrp_new_gts() %>%
      wrp_set_attributes(attr = "test") %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_relabel(foo = "bar") %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_set_attributes(attr = "test") %>%

      wrp_new_gts() %>%
      wrp_relabel(foo = "bar") %>%
      wrp_set_attributes(attr = "test") %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_relabel(foo = "bar") %>%
      wrp_set_attributes(attr = "test") %>%

      wrp_exec() %>%
      bind_lgts()

  })
})

test_that("Bind LGTS with warning", {
  expect_warning({
    wrp_connect() %>%
      wrp_new_gts() %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%

      wrp_new_gts() %>%
      wrp_relabel(foo = "bar") %>%

      wrp_new_gts() %>%
      wrp_set_attributes(attr = "test") %>%

      wrp_new_gts() %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_relabel(foo = "bar") %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_set_attributes(attr = "test") %>%

      wrp_new_gts() %>%
      wrp_relabel(foo = "bar") %>%
      wrp_set_attributes(attr = "test") %>%

      wrp_new_gts() %>%
      wrp_relabel(foo = "bar") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_set_attributes(attr = "test") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_relabel(foo = "bar") %>%
      wrp_set_attributes(attr = "test") %>%

      wrp_new_gts() %>%
      wrp_relabel(foo = "bar") %>%
      wrp_set_attributes(attr = "test") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_set_attributes(attr = "test") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_relabel(foo = "bar") %>%
      wrp_add_value(0, 0) %>%

      wrp_new_gts() %>%
      wrp_rename("class") %>%
      wrp_relabel(foo = "bar") %>%
      wrp_set_attributes(attr = "test") %>%
      wrp_add_value(0, 0) %>%

      wrp_exec() %>%
      bind_lgts()

  })
})


