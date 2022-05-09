library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("get_template")

test_that(
  "get_template",
  {
    y <- get_template("GERMAPARLMINI")
    expect_identical(length(y), 3L)
    expect_identical(unname(y$metadata), c("speaker", "date", "party"))
    
    p <- partition("GERMAPARLMINI", date = "2009-11-11")
    y <- get_template(p)
    expect_identical(length(y), 3L)
    expect_identical(unname(y$metadata), c("speaker", "date", "party"))
  }
)

