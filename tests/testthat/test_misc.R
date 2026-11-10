library(polmineR)
use(pkg = "RcppCWB", corpus = "REUTERS")

test_that("count_one_query", {expect_equal(size("REUTERS"), 4050)})

