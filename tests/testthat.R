library(testthat)
library(warp10r)

ctnconfig::load_config("qual")
test_check("warp10r")
