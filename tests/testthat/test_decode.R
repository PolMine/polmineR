library(polmineR)
testthat::context("decode")
use("polmineR")


test_that(
  "decode entire corpus",
  {
    dt <- decode("GERMAPARLMINI")
    expect_equal(ncol(dt), 7)
    expect_equal(nrow(dt), 222201)
    expect_equal(dt[["word"]][1:6], c("Guten", "Morgen", ",", "meine", "sehr", "verehrten"))
    expect_equal(length(unique(dt[["date"]])), 5)
  }
)

