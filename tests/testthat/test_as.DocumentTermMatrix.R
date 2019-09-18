library(polmineR)
use("polmineR")
testthat::context("as.TermDocumentMatrix")

test_that(
  "Generate Term-Document-Matrix from corpus using as.TermDocumentMatrix",
  {
    dtm <- as.DocumentTermMatrix("REUTERS", p_attribute = "word", sAttribute = "id")
    expect_equal(
      length(sAttributes("REUTERS", "id")), dim(dtm)[1]
    )
    expect_equal(
      RcppCWB::cl_lexicon_size(corpus = "REUTERS", p_attribute = "word", registry = registry()),
      dim(dtm)[2]
    )
    expect_equal(sum(dtm[,"is"]), count("REUTERS", "is")[["count"]])
  }
)


test_that(
  "identity of as.TermDocumentMatrix and as.DocumentTermMatrix",
  {
    sp <- as.speeches("GERMAPARLMINI", s_attribute_name = "speaker")
    
    tdm <- as.TermDocumentMatrix(sp, p_attribute = "word")
    dtm <- as.DocumentTermMatrix(sp, p_attribute = "word")
    
    expect_identical(tdm, t(dtm))
  }
)