library(polmineR)
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("features")

test_that(
  "formatting features object",
  {
    x <- partition("REUTERS", places = "qatar", p_attribute = "word")
    z <- features(x, "REUTERS", included = TRUE)
    
    expect_identical(
      colnames(format(z)),
      c("rank_chisquare", "word", "count_coi", "count_ref", "exp_coi", "chisquare")
    )
    
  }
)

test_that(
  "formatting features_ngrams object",
  {
    a <- partition("REUTERS", id = "127") %>%
      ngrams(n = 2, p_attribute = "word")
    
    b <- ngrams("REUTERS", n = 2, p_attribute = "word")
    
    f <- features(a, b, included = TRUE, verbose = FALSE)
    
    expect_identical(
      colnames(format(f)),
      c(
        "rank_chisquare", "word_1", "word_2", "count_coi", "count_ref",
        "exp_coi", "chisquare"
      )
    )
  }
)


