library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")
testthat::context("pmi")

test_that(
  "check calculation of pointwise mutual information",
  {
    y <- cooccurrences("REUTERS", query = "oil", method = "pmi")
    N <- size(y)[["ref"]] + size(y)[["coi"]] + count(y)
    I <- log2((y[["count_coi"]]/N) / ((count(y) / N) * (y[["count_partition"]] / N)))
    
    expect_equal(y[["pmi"]], I, tolerance = 1e-3)
  }
)

test_that(
  "identity of phrase detection of decode-workflow and Cooccurrences workflow",
  {
    a <- corpus("GERMAPARLMINI") %>%
      decode(p_attribute = "word", s_attribute = character(), to = "data.table", verbose = FALSE) %>%
      ngrams(n = 2L, p_attribute = "word") %>%
      pmi(observed = count("GERMAPARLMINI", p_attribute = "word"))
    
    b <- Cooccurrences("GERMAPARLMINI", p_attribute = "word", left = 0L, right = 1L, verbose = FALSE) %>%
      decode() %>%
      pmi()
    
    a_min <- subset(a, ngram_count == 5L) %>% slot("stat") %>% data.table::setorderv(cols = c("word_1", "word_2"))
    b_min <- subset(b, ab_count == 5L) %>% slot("stat") %>% data.table::setorderv(cols = c("a_word", "b_word"))
    
    expect_identical(nrow(a_min), nrow(b_min))
    expect_identical(a_min[["word_1"]], b_min[["a_word"]])
    expect_identical(a_min[["word_2"]], b_min[["b_word"]])
  }
)
