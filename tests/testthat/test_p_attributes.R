library(polmineR)

use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("p_attributes")

test_that(
  "p_attributes,character",
  {
    expect_identical(
      p_attributes("GERMAPARLMINI"),
      c("word", "pos")
    )
  }
)


test_that(
  "p_attributes,corpus",
  {
    expect_identical(
      p_attributes(corpus("GERMAPARLMINI")),
      c("word", "pos")
    )
    expect_identical(
      length(p_attributes(corpus("REUTERS"), p_attribute = "word")),
      RcppCWB::cl_lexicon_size("REUTERS", p_attribute = "word", registry = registry())
    )
  }
)
