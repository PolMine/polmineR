library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("subcorpus")

test_that(
  "message on registry used if corpus is loaded multiple times",
  {
    library(RcppCWB)
    expect_identical(
      length(RcppCWB::corpus_registry_dir("REUTERS")),
      2L
    )
    expect_message(corpus("REUTERS"))
  }
)
