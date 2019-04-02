library(polmineR)
use("polmineR")
testthat::context("as.speeches")

test_that(
  "as.speeches",
  {
    p <- partition("GERMAPARLMINI", date = ".*", regex = TRUE)
    pb <- as.speeches(p, s_attribute_name = "speaker")
    expect_equal(length(pb), 276L)
    
    scb <- as.speeches(corpus("GERMAPARLMINI"), s_attribute_name = "speaker")
    expect_equal(length(scb), length(pb))
    expect_equal(sum(unname(unlist(lapply(scb@objects, size)))), size("GERMAPARLMINI"))
    expect_equal(all(names(scb) %in% names(pb)), TRUE)
    
    pb <- pb[[names(scb)]]
    expect_identical(names(scb), names(pb))
    expect_identical(
      do.call(rbind, lapply(scb@objects, function(x) x@cpos)),
      do.call(rbind, lapply(pb@objects, function(x) x@cpos))
    )
  }
)

test_that(
  "tdm for as.speeches",
  {
    p <- partition("GERMAPARLMINI", date = ".*", regex = TRUE)
    pb <- as.speeches(p, s_attribute_name = "speaker")
    cnt <- count(pb, p_attribute = "word")
    tdm <- as.TermDocumentMatrix(cnt, col = "count")
    
    co <- corpus("GERMAPARLMINI")
    sp <- as.speeches(co, s_attribute_name = "speaker")
    tmp <- sp[[names(pb)]]
    sp@objects <- tmp@objects
    cnt2 <- count(sp, p_attribute = "word")
    tdm2 <- as.TermDocumentMatrix(cnt2, col = "count")
    
    expect_identical(tdm, tdm2)
  }
)

test_that(
  "tdm for as.speeches, but partition/subcorpus",
  {
    p <- partition("GERMAPARLMINI", date = "2009-11-11", regex = TRUE)
    pb <- as.speeches(p, s_attribute_name = "speaker")
    cnt <- count(pb, p_attribute = "word")
    tdm <- as.TermDocumentMatrix(cnt, col = "count")
    
    co <- corpus("GERMAPARLMINI")
    s <- subset(co, date == "2009-11-11")
    sp <- as.speeches(co, s_attribute_name = "speaker")
    tmp <- sp[[names(pb)]]
    sp@objects <- tmp@objects
    cnt2 <- count(sp, p_attribute = "word")
    tdm2 <- as.TermDocumentMatrix(cnt2, col = "count")
    
    expect_identical(tdm, tdm2)
  }
)