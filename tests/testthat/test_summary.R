library(polmineR)
testthat::context("summary")
use("polmineR")


test_that(
  "summary,partition-method",{
    p <- partition("GERMAPARLMINI", date = "2009-11-11")
    p <- enrich(p, p_attribute = "word")
    weights <- data.table(
      word = c("gut", "super", "herrlich", "schlecht", "grob", "mies"),
      weight = c(1,1,1,-1,-1,-1)
    )
    p <- weigh(p, with = weights)
    p_summary <- summary(p)
    expect_equal(p_summary$negative_weighed, -7L)
    
  }
)


test_that(
  "summary,count-method",{
    weights <- data.table(
      word = c("gut", "super", "herrlich", "schlecht", "grob", "mies"),
      weight = c(1,1,1,-1,-1,-1)
    )
    corp <- corpus("GERMAPARLMINI")
    sc <- subset(corp, date == "2009-11-11")
    cnt <- count(sc, p_attribute = "word")
    cnt <- weigh(cnt, with = weights)
    y <- summary(cnt)
    expect_equal(y$negative_weighed, -7L)
    
  }
)

