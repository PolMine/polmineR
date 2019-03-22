library(polmineR)
use("polmineR")

testthat::context("ngrams-method")

test_that("ngrams",{
  o <- corpus("REUTERS")
  n <- ngrams(o, n = 2)
  dt <- data.table::as.data.table(n)
  data.table::setorderv(dt, cols = "count", order = -1L)
  dt_min <- dt[!dt[["1_word"]] %in% tm::stopwords("en")]
  for (i in 1L:5L){
    n <- count(
      corpus("REUTERS"),
      query = sprintf('"%s" "%s"', dt_min[["1_word"]][i], dt_min[["2_word"]][i])
    )[["count"]]
    expect_equal(n, dt_min[["count"]][i])
  }
  
  
  p <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
  n <- ngrams(p, n = 2)
  dt <- data.table::as.data.table(n)
  setorderv(dt, cols = "count", order = -1L)
  dt_min <- dt[!dt[["1_word"]] %in% tm::stopwords("en")]
  for (i in 1L:5L){
    n <- count(
      p,
      query = sprintf('"%s" "%s"', dt_min[["1_word"]][i], dt_min[["2_word"]][i])
    )[["count"]]
    expect_equal(n, dt_min[["count"]][i])
  }
})
