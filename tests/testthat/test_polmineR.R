library(polmineR)

testthat::context("testing polmineR")

if (requireNamespace("polmineR.sampleCorpus", quietly = TRUE)){
  use("polmineR.sampleCorpus")
  test_that("", {
    expect_equal(length(sAttributes("PLPRBTTXT", "text_date")), 5)
  })
}

