library(polmineR)

testthat::context("as.regions")

test_that(
  "coerce to regions",
  {
    p <- partition("GERMAPARLMINI", speaker = "Angela Dorothea Merkel")
    r1 <- as(p, "regions")
    
    sc <- subset("GERMAPARLMINI", speaker == "Angela Dorothea Merkel")
    r2 <- as(sc, "regions")
    
    r2@data_dir <- character()
    expect_identical(r1, r2)
  }
)

