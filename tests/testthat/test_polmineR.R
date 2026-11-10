library(polmineR)

use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

test_that(
  "corpora present",
  {
    expect_equal("GERMAPARLMINI" %in% corpus()[["corpus"]], TRUE)
    expect_equal("REUTERS" %in% corpus()[["corpus"]], TRUE)
  }
)
