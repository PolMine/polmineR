library(polmineR)
testthat::context("enrich")
use("RcppCWB")


test_that(
  "trim context",
  {
    oil <- context("REUTERS", query = "oil")
    oil_no_crude <- polmineR:::trim(oil, stoplist = "crude")
    expect_identical(length(oil_no_crude), 65L)
    
    oil <- polmineR::context("REUTERS", query = "oil")
    oil_just_crude <- polmineR:::trim(oil, positivelist = "crude")
    expect_identical(length(oil_just_crude), 78L - 65L)
    
  }
)

