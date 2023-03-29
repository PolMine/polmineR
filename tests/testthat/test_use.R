library(polmineR)
testthat::context("use")

test_that(
  "check return values of use()",
  {
    retval <- use("abcdefg", verbose = FALSE)
    expect_false(retval)

    retval <- use("testthat", verbose = FALSE)
    expect_false(retval)
    
    retval <- use("polmineR", corpus = "REUTERS", verbose = FALSE)
    expect_false(retval)
    
    retval <- use("polmineR", corpus = "GERMAPARLMINI", verbose = FALSE)
    expect_true(retval)
    
    expect_true("GERMAPARLMINI" %in% RcppCWB::cqp_list_corpora())
  }
)
