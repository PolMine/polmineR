library(polmineR)
use("polmineR")
testthat::context("cooccurrences")

test_that(
  "cooccurrences-method for corpus",
  {
    expect_equal(
      cooccurrences("REUTERS", query = "oil", pAttribute = "word")@stat[["word"]][1:5],
      c("prices", "crude", "world", "markets", "industry")
    )
    
    expect_equal(
      cooccurrences("REUTERS", query = '"barrel.*"', pAttribute = "word")@stat[["word"]][1:5],
      c("dlrs", "mln", "a", "18", "per")
    )
    
    expect_equal(
      cooccurrences("REUTERS", query = "asdfasdf", pAttribute = "word"),
      NULL
    )
    
    expect_equal(
      cooccurrences("REUTERS", query = '"asdfasdfasdfasd.*"', cqp = TRUE),
      NULL
    )
  }
)

test_that(
  "cooccurrences-method for partition",
  {
    P <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
    
    expect_equal(
      cooccurrences(P, query = "oil", pAttribute = "word")@stat[["word"]][1:5],
      c("prices", "each", "market", "other", "crude")
    )
    
    expect_equal(
      cooccurrences(P, query = '"barrel.*"', cqp = TRUE, pAttribute = "word")@stat[["word"]][1:5],
      c("a", "dlrs", "18", "day","per")
    )
    
    expect_equal(
      cooccurrences(P, query = "asdfasdf", pAttribute = "word"),
      NULL
    )
    
    expect_equal(
      cooccurrences(P, query = '"asdfasdfasdfasd.*"', cqp = TRUE, pAttribute = "word"),
      NULL
    )
  }
)


test_that(
  "Identity of cooccurrences and Cooccurrences",
  {
    stopwords <- unname(unlist(noise(terms("REUTERS", p_attribute = "word"), stopwordsLanguage = "en")))
    r <- Cooccurrences("REUTERS", p_attribute = "word", left = 5L, right = 5L, stoplist = stopwords)
    ll(r)
    decode(r)

    a <- data.table::as.data.table(cooccurrences(r, query = "oil"))
    b <- data.table::as.data.table(cooccurrences("REUTERS", query = "oil"))[!word %in% stopwords]

    expect_equal(a[["word"]][1:14], b[["word"]][1:14])
    expect_equal(a[["count_partition"]], b[["count_partition"]])
    expect_equal(a[["count_window"]], b[["count_window"]])
    expect_equal(a[["exp_window"]], b[["exp_window"]])
    expect_equal(a[["exp_partition"]], b[["exp_partition"]])
    expect_equal(a[["ll"]], b[["ll"]])
  }
)


