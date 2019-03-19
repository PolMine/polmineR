library(polmineR)
use("polmineR")
testthat::context("hits")

test_that(
  "hits method",
  {
    y <- hits("REUTERS", query = "oil")
    expect_equal(nrow(y), count("REUTERS", query = "oil")[["count"]])
    
    y <- hits("REUTERS", query = "oil", s_attribute = "places", freq = TRUE)
    expect_equal(y@stat[places == "argentina"][["count"]], 1L)

    p <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
    y <- hits(p, query = "oil")
    expect_equal(nrow(y), count(p, query = "oil")[["count"]])
    
    y <- hits(p, query = "oil", s_attribute = "id")
    expect_equal(y@stat[id == "242"][["count"]], count(partition("REUTERS", id = "242"), query = "oil")[["count"]])
  }
)
