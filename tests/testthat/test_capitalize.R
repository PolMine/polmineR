library(polmineR)
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("capitalize")

test_that("",
  {
  vec <- c("oil", "data", "corpus", NA, "x")
  result <- capitalize(vec)
  expect_identical(result[1], "Oil")
  expect_identical(result[2], "Data")
  expect_identical(result[3], "Corpus")
  expect_true(is.na(result[4]))
  expect_identical(result[5], "X")
})

