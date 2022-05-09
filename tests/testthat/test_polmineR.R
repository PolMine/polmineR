library(polmineR)

use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("testing polmineR")

test_that(
  "corpora present",
  {
    expect_equal("GERMAPARLMINI" %in% corpus()[["corpus"]], TRUE)
    expect_equal("REUTERS" %in% corpus()[["corpus"]], TRUE)
  }
)
