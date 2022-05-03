library(polmineR)
use("polmineR")
testthat::context("features")

test_that(
  "features (comparing a partition with a partition)",
  {
    x <- partition("REUTERS", places = "qatar", p_attribute = "word")
    y <- partition("REUTERS", places = ".*", regex = TRUE, p_attribute = "word")
    z <- features(x, y, included = TRUE)
    expect_equal(
      z@stat[["word"]][1:5], c("budget", "riyals", "billion", "Abdul", "Aziz")
      )
    expect_equal(sum(z@stat[["count_coi"]][1:5]), 29)  
  }
)

test_that(
  "features (comparing a partition with corpus)",
  {
    x <- partition("REUTERS", places = "qatar", p_attribute = "word")
    z <- features(x, "REUTERS", included = TRUE)
    expect_equal(
      z@stat[["word"]][1:5],
      c("budget", "riyals", "billion", "Abdul", "Aziz")
    )
    expect_equal(
      sum(z@stat[["count_coi"]][1:5]),
      29
    )
  }
)


test_that(
  "features (comparing ngrams with ngrams)",
  {
    a <- partition("REUTERS", places = "qatar", p_attribute = "word")
    b <- partition("REUTERS", places = ".*", regex = TRUE, p_attribute = "word")
    x <- ngrams(a, p_attribute = "word")
    y <- ngrams(b, p_attribute = "word")
    z <- features(x, y)
    expect_equal(
      z@stat[["word_1"]][1:5], c("billion", "Abdul", "Sheikh", "Aziz", "1985")
    )
    expect_equal(
      z@stat[["word_2"]][1:5], c("riyals", "Aziz", "Abdul", "said", "86")
    )
  }
)

test_that(
  "features (comparing count with count)",
  {
    a <- partition("REUTERS", places = "qatar", p_attribute = "word")
    x <- as(a, "count")
    b <- partition("REUTERS", places = ".*", regex = TRUE, p_attribute = "word")
    y <- as(b, "count")
    z <- features(x, y, included = TRUE)
    expect_equal(
      z@stat[["word"]][1:5], c("budget", "riyals", "billion", "Abdul", "Aziz")
    )
    expect_equal(sum(z@stat[["count_coi"]][1:5]), 29)  
  }
)