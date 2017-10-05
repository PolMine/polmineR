library(polmineR)

testthat::context("count-method")

if (requireNamespace("polmineR.sampleCorpus", quietly = TRUE)){
  use("polmineR.sampleCorpus")
  debates <- partition("PLPRBTTXT", list(text_id=".*"), regex = TRUE)
  test_that("count_one_query", {
    expect_equal(count(debates, query = "Arbeit")[["count"]], 159)
  })
  test_that("multiple_queries", {
    expect_equal(
      count(debates, c("Arbeit", "Freizeit", "Zukunft"))[["count"]],
      c(159, 1, 142)
    )
  })
  
}

