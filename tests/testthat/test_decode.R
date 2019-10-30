library(polmineR)
testthat::context("decode")
use("polmineR")


test_that(
  "decode entire corpus",
  {
    dt <- decode("GERMAPARLMINI", to = "data.table")
    expect_equal(ncol(dt), 7L)
    expect_equal(nrow(dt), 222201L)
    expect_equal(dt[["word"]][1:6], c("Guten", "Morgen", ",", "meine", "sehr", "verehrten"))
    expect_equal(length(unique(dt[["date"]])), 5L)
  }
)

