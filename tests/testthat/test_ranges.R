library(polmineR)
use("polmineR")
testthat::context("ranges")

test_that(
  "hits method",
  {
    rng_y <- ranges("REUTERS", query = "oil", progress = FALSE)
    expect_equal(nrow(rng_y@cpos), count("REUTERS", query = "oil")[["count"]])
    
    sc <- corpus("REUTERS") %>% subset(grep("saudi-arabia", places))
    expect_equal(nrow(ranges(sc, query = "oil")@cpos), count(sc, query = "oil")[["count"]])
    
    p <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
    y <- hits(p, query = "oil")
    expect_equal(nrow(ranges(p, query = "oil")@cpos), as.data.table(y)[["count"]])
    
    y <- hits("REUTERS", query = c(a = "oil", b = "barrel"))
    rng <- ranges("REUTERS", query = c(a = "oil", b = "barrel"))
    identical(as.data.table(y), as.data.table(rng))
    
    expect_error(ranges("REUTERS", query = c(a = "oil", "barrel")))
  }
)
