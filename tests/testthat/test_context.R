library(polmineR)
use("polmineR")
testthat::context("context")


test_that(
  "context-method for corpus",
  {
    y <- polmineR::context("REUTERS", query = "oil", pAttribute = "word")@stat
    expect_equal(colnames(y), c("word_id", "count_coi", "word"))
    expect_equal(sum(y[["count_coi"]]), 780L)
    
    y <- polmineR::context("REUTERS", query = '"barrel.*"', pAttribute = "word")@stat
    expect_equal(colnames(y), c("word_id", "count_coi", "word"))
    expect_equal(sum(y[["count_coi"]]), 260L)
    
    y <- polmineR::context("REUTERS", query = "asdfasdf", pAttribute = "word")
    expect_equal(y, NULL)
    
    y <- polmineR::context("REUTERS", query = '"asdfasdfasdfasd.*"', cqp = TRUE, pAttribute = "word")
    expect_equal(y, NULL)
  }
)

test_that(
  "context-method for partition",
  {
    P <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
    
    y <- polmineR::context(P, query = "oil", pAttribute = "word")@stat
    expect_equal(colnames(y), c("word_id", "count_coi", "word"))
    expect_equal(sum(y[["count_coi"]]), 210L)
    
    y <- polmineR::context(P, query = '"barrel.*"', pAttribute = "word")@stat
    expect_equal(colnames(y), c("word_id", "count_coi", "word"))
    expect_equal(sum(y[["count_coi"]]), 70L)
    
    y <- polmineR::context(P, query = "asdfasdf", pAttribute = "word")
    expect_equal(y, NULL)
    
    y <- polmineR::context(P, query = '"asdfasdfasdfasd.*"', cqp = TRUE, pAttribute = "word")
    expect_equal(y, NULL)
  }
)

test_that(
  "context-method for subcorpus_bundle",
  {
    # This is a somewhat limited test: We check that positivelist and stoplist
    # are applied as intended
    
    q <- "Arbeit"
    positive <- "gute"
    
    scb <- corpus("GERMAPARLMINI") %>%
      as.speeches(, s_attribute_name = "speaker", progress = FALSE)

    p <- polmineR::context(
      scb, query = q, p_attribute = "word", positivelist = positive,
      verbose = FALSE
    )
    
    expect_true(
      all(
        sapply(
          lapply(lapply(p@objects, slot, "stat"), `[[`, "word"),
          function(vec) positive %in% vec
        )
      )
    )
    
    n <- polmineR::context(
      scb, query = q, p_attribute = "word", stoplist = positive,
      verbose = FALSE
    )
    
    expect_true(
      all(
        sapply(
          lapply(lapply(n@objects, slot, "stat"), `[[`, "word"),
          function(vec) !positive %in% vec
        )
      )
    )
  }
)
