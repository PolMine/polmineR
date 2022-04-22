library(polmineR)
use("polmineR")
testthat::context("as.speeches")

test_that(
  "as.speeches",
  {
    p <- partition("GERMAPARLMINI", date = ".*", regex = TRUE)
    pb <- as.speeches(corpus("GERMAPARLMINI"), s_attribute_name = "speaker")
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
  "as.speeches() same result for partition and corpus-method",
  {
    sp_all <- as.speeches("GERMAPARLMINI", s_attribute_name = "speaker")
    sp_min1 <- sp_all[[grep("(2009-10-28|2009-11-10)", names(sp_all), value = TRUE)]]
    
    sp_min2 <- corpus("GERMAPARLMINI") %>%
      subset(date %in% c("2009-10-28", "2009-11-10")) %>%
      as.speeches(s_attribute_name = "speaker")
    
    expect_identical(length(sp_min1), length(sp_min2))
    expect_identical(sum(summary(sp_min1)$size), sum(summary(sp_min2)$size))
    expect_true(all(names(sp_min1) %in% names(sp_min2)))
    
  }
)

test_that(
  "tdm for as.speeches",
  {
    skip("knowingly not working")
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

