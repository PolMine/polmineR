library(polmineR)
testthat::context("html")
use("polmineR")


test_that(
  "enrich partition",
  {
    testthat::skip_on_cran()
    g_min <- corpus("GERMAPARLMINI")
    g_min_sub <- subset(g_min, date == "2009-11-11")
    g_min_sub_speeches <- as.speeches(g_min_sub, s_attribute_name = "speaker", s_attribute_date = "date", progress = FALSE)
    output <- html(g_min_sub_speeches, progress = TRUE, beautify = FALSE)
    expect_equal(is(output)[1], "html")
    }
)

