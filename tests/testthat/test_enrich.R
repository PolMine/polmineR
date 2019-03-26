library(polmineR)
testthat::context("enrich")
use("polmineR")


test_that(
  "enrich partition",
  {
    x <- partition("GERMAPARLMINI", date = "2009-11-11", speaker = "Gerda Hasselfeldt")
    y <- enrich(x, p_attribute = "word")
    cnt <- count(x, p_attribute = "word")
    expect_identical(y@stat, cnt@stat)
  }
)

