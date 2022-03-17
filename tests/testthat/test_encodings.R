library(polmineR)
use("polmineR")
testthat::context("encoding")

test_that(
  "as.corpusEnc",
  {
    mixed <- c("ä", "ö", "ü")
    Encoding(mixed)[2] <- "latin1"
    testthat::expect_warning(as.corpusEnc(mixed, from = "UTF-8", corpusEnc = "latin1"))
  }
)

