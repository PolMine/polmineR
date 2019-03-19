library(polmineR)

testthat::context("dispersion")

test_that(
  "dispersion",
  {
    y <- dispersion("GERMAPARLMINI", query = "Integration", s_attribute = "date")
    expect_identical(y[["count"]], c(1L, 7L, 15L))

    p <- partition("GERMAPARLMINI", date = "2009-11-11", p_attribute = NULL, regex = TRUE)
    int <- dispersion(p, query = "Integration", p_attribute = "word", s_attribute = "speaker")
    expect_identical(int[speaker == "Hartfrid Wolff"][["count"]], 3L)
    expect_identical(int[speaker == "Hermann Otto Solms"][["count"]], 4L)
    
    int <- dispersion("GERMAPARLMINI", "Integration", s_attribute = c("date", "party"))
    expect_equal(rowSums(int[, 2:ncol(int)]), y[["count"]])
    
    integration <- dispersion("GERMAPARLMINI", '"Integration.*"', s_attribute = "date", cqp = TRUE)
    expect_equal(integration[["count"]], c(1L, 11L, 31L))
  }
)

