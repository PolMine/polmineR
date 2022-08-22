library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")
testthat::context("as.TermDocumentMatrix")

test_that(
  "Generate Term-Document-Matrix from corpus using as.TermDocumentMatrix",
  {
    dtm <- as.DocumentTermMatrix("REUTERS", p_attribute = "word", s_attribute = "id")
    expect_equal(
      length(s_attributes("REUTERS", "id")), dim(dtm)[1]
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
    sp <- as.speeches("GERMAPARLMINI", s_attribute_name = "speaker", s_attribute_date = "date")
    
    tdm <- as.TermDocumentMatrix(sp, p_attribute = "word")
    dtm <- as.DocumentTermMatrix(sp, p_attribute = "word")
    
    expect_identical(tdm, t(dtm))
  }
)

test_that(
  "Check ways to generate DocumentTermMatrix against each other",
  {
    pb <- partition_bundle("GERMAPARLMINI", s_attribute = "speaker")
    
    dtm_count <- count(pb, p_attribute = "word", verbose = FALSE) %>%
      as.sparseMatrix(col = "count")
    
    dtm_enrich <- enrich(pb, p_attribute = "word") %>%
      as.sparseMatrix(col = "count")
    
    dtm_direttisima <- as.DocumentTermMatrix("GERMAPARLMINI", p_attribute = "word", s_attribute = "speaker") %>%
      as.sparseMatrix() %>%
      Matrix::t()
      
    expect_identical(length(which(!rownames(dtm_direttisima) %in% rownames(dtm_count))), 0L)
    expect_identical(length(which(!rownames(dtm_enrich) %in% rownames(dtm_count))), 0L)
    expect_identical(length(which(!colnames(dtm_enrich) %in% colnames(dtm_count))), 0L)
    
    expect_identical(slam::col_sums(dtm_enrich), slam::col_sums(dtm_count))
    

    dtm_enrich2 <- dtm_enrich[rownames(dtm_count),]
    expect_identical(dtm_count, dtm_enrich2)
    
    dtm_direttisima2 <- dtm_direttisima[,colnames(dtm_count)]
    dtm_direttisima3 <- dtm_direttisima2[rownames(dtm_count),]

    expect_identical(dtm_count, dtm_direttisima3)
    
  }
)