library(polmineR)

testthat::context("count-method")

reuters <- partition("REUTERS", list(id = ".*"), regex = TRUE)
test_that("count_one_query", {
  expect_equal(count(reuters, query = "is")[["count"]], 25)
})
test_that("multiple_queries", {
  expect_equal(
    count(reuters, c("is", "this", "real"))[["count"]],
    c(25, 7, 3)
  )
})
