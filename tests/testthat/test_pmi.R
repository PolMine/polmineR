library(polmineR)
use("polmineR")
testthat::context("pmi")

test_that(
  "check calculation of pointwise mutual information",
  {
    y <- cooccurrences("REUTERS", query = "oil", method = "pmi")
    N <- size(y)[["ref"]] + size(y)[["coi"]] + count(y)
    I <- log2((y[["count_coi"]]/N) / ((count(y) / N) * (y[["count_partition"]] / N)))
    
    expect_equal(y[["pmi"]], I, tolerance = 1e-3)
  }
)
