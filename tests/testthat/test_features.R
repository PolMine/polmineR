library(polmineR)
testthat::context("features")

test_that(
  "features (comparing a partition with a partition)",
  {
    x <- partition("REUTERS", places = "qatar", pAttribute = "word")
    y <- partition("REUTERS", places = ".*", regex = TRUE, pAttribute = "word")
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
    x <- partition("REUTERS", places = "qatar", pAttribute = "word")
    z <- features(x, "REUTERS", included = TRUE)
    expect_equal(
      z@stat[["word"]][1:5], c("budget", "riyals", "billion", "Abdul", "Aziz")
    )
    expect_equal(sum(z@stat[["count_coi"]][1:5]), 29)  
  }
)


test_that(
  "features (comparing ngrams with ngrams)",
  {
    a <- partition("REUTERS", places = "qatar", pAttribute = "word")
    b <- partition("REUTERS", places = ".*", regex = TRUE, pAttribute = "word")
    x <- ngrams(a, pAttribute = "word")
    y <- ngrams(b, pAttribute = "word")
    z <- features(x, y)
    expect_equal(
      z@stat[["1_word"]][1:5], c("of", "oil", "said", "world", "said")
    )
    expect_equal(
      z@stat[["2_word"]][1:5], c("the", "prices", "the", "oil", "that")
    )
  }
)
