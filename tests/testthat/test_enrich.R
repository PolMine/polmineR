library(polmineR)
testthat::context("enrich")
use("polmineR")
use("RcppCWB")

test_that(
  "enrich partition",
  {
    x <- partition("GERMAPARLMINI", date = "2009-11-11", speaker = "Gerda Hasselfeldt")
    y <- enrich(x, p_attribute = "word")
    cnt <- count(x, p_attribute = "word")
    expect_identical(y@stat, cnt@stat)
  }
)


test_that(
  "enrich partition_bundle",
  {
    pb <- partition_bundle("REUTERS", s_attribute = "id")
    pb_word <- enrich(pb, p_attribute = "word")
  }
)
