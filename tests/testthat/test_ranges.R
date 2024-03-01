library(polmineR)
use(pkg = "RcppCWB", corpus = "REUTERS")
testthat::context("ranges")

test_that(
  "hits method",
  {
    rng_y <- ranges("REUTERS", query = "oil", progress = FALSE)
    expect_equal(nrow(rng_y@cpos), count("REUTERS", query = "oil")[["count"]])
    
    rng_nohits <- ranges(corpus("REUTERS"), query = '"adf"', cqp = TRUE)
    expect_identical(nrow(rng_nohits@cpos), 0L)
    expect_identical(rng_nohits@size, 0L)
    expect_identical(rng_nohits@match, character())
    expect_identical(rng_nohits@query, character())
    expect_identical(nrow(as.data.table(rng_nohits)), 0L)
    
    rng_nohits <- corpus("REUTERS") %>% 
      subset(id == "127") %>%
      ranges(query = '"adf"', cqp = TRUE)
    expect_identical(nrow(rng_nohits@cpos), 0L)
    expect_identical(rng_nohits@size, 0L)
    expect_identical(rng_nohits@match, character())
    expect_identical(rng_nohits@query, character())
    expect_identical(nrow(as.data.table(rng_nohits)), 0L)
    
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
