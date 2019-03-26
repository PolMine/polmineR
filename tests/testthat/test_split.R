library(polmineR)
testthat::context("split")
use("polmineR")


test_that(
  "split up corpus",
  {
    x <- partition("GERMAPARLMINI", date = "2009-11-11", speaker = "Gerda Hasselfeldt")
    y <- split(x, gap = 500)
    expect_identical(is(y)[1], "partition_bundle")
    expect_identical(length(y), 15L)
    expect_identical(size(merge(y)), size(x))
    
    y2 <- merge(y)
    expect_identical(as.integer(merge(y)@cpos), as.integer(x@cpos))
    expect_identical(as.vector(merge(y)@cpos), as.vector(x@cpos))
  }
)

