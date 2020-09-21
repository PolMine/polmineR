library(polmineR)
testthat::context("registry_eval")
use("polmineR")


test_that(
  "extract encoding from registry file",
  {
    expect_identical(registry_get_encoding("REUTERS"), RcppCWB::cl_charset_name("REUTERS"))
  }
)
