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
    
    ## this is a more comprehensive test that ensures that a column for a document
    # from the document-term-matrix and a simple count for this document are identical
    
    dtm <- as.DocumentTermMatrix("GERMAPARLMINI", p_attribute = "word", s_attribute = "party")
    spd_cnt <- as.matrix(dtm)["SPD",]
    spd_dt <- data.table::data.table(token = names(spd_cnt), count = unname(spd_cnt))[count > 0L]
    data.table::setorderv(spd_dt, cols = "token")
    
    spd_dt_obj <- corpus("GERMAPARLMINI") %>% subset(party == "SPD") %>% count(p_attribute = "word")
    spd_cnt_2 <- spd_dt_obj@stat[, "word_id" := NULL]
    data.table::setorderv(spd_cnt_2, cols = "word")
    
    expect_identical(spd_cnt_2[["count"]], spd_dt[["count"]])
    expect_identical(spd_cnt_2[["word"]], spd_dt[["token"]])

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