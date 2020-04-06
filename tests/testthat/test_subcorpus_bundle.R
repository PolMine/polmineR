library(polmineR)
use("polmineR")
testthat::context("subcorpus_bundle")

test_that(
  "get TermDocumentMatrix from subcorpus_bundle",
  {
    tdm <- corpus("REUTERS") %>%
      split(s_attribute = "id") %>%
      as.TermDocumentMatrix(p_attribute = "word")
    
    tdm2 <- partition_bundle("REUTERS", s_attribute = "id") %>%
      enrich(p_attribute = "word") %>%
      as.TermDocumentMatrix(col = "count")
    
    expect_identical(tdm, tdm2)
  }
)

test_that(
  "generate VCorpus from subcorpus_bundle",
  {
    vcorp1 <- partition_bundle("REUTERS", s_attribute = "id") %>%
      as.VCorpus()

    vcorp2 <- corpus("REUTERS") %>%
      split(s_attribute = "id") %>%
      as.VCorpus()

    expect_identical(vcorp1, vcorp2)
  }
)

test_that(
  "summary for subcorpus_bundle",
  {
    s1 <- partition_bundle("REUTERS", s_attribute = "id") %>%
      summary()
    
    s2 <- corpus("REUTERS") %>% split(s_attribute = "id") %>% summary()
    
    expect_identical(s1, s2)
  }
)

test_that(
  "count over subcorpus_bundle",
  {
    cnt1 <- partition_bundle("REUTERS", s_attribute = "id") %>% count(query = "oil")
    cnt2 <- corpus("REUTERS") %>% split(s_attribute = "id") %>% count(query = "oil")
    
    expect_identical(cnt1, cnt2)
  }
)

test_that(
  "from subcorpus to count_bundle",
  {
    ref <- corpus("GERMAPARLMINI") %>% count(p_attribute = "word")
    cois <- corpus("GERMAPARLMINI") %>%
      subset(speaker %in% c("Angela Dorothea Merkel", "Hubertus Heil")) %>%
      split(s_attribute = "speaker") %>%
      count(p_attribute = "word")
    y1 <- features(cois, ref, included = TRUE, method = "chisquare", progress = TRUE)
    
    pb <- partition_bundle("GERMAPARLMINI", s_attribute = "speaker")
    
    pb <- pb[[c("Angela Dorothea Merkel", "Hubertus Heil")]]
    pb <- enrich(pb, p_attribute = "word")
    y2 <- features(pb, "GERMAPARLMINI", included = TRUE, method = "chisquare")
    
    for (i in seq_along(y1@objects)) y1@objects[[i]]@stat[, "id" := NULL]
    
    for (i in seq_along(y2@objects)) y2@objects[[i]]@stat[, "word_id.y" := NULL]
    for (i in seq_along(y2@objects)) data.table::setnames(y2@objects[[i]]@stat, old = "word_id.x", new = "word_id")
    for (i in seq_along(y2@objects)) data.table::setcolorder(y2@objects[[i]]@stat, neworder = colnames(y1@objects[[i]]@stat))

    expect_identical(y1, y2)
    
  }
)

test_that(
  "get_type-method for subcorpus_bundle",
  {
    expect_identical(
      corpus("GERMAPARLMINI") %>% split(s_attribute = "speaker") %>% get_type(),
      "plpr"
    )
    
  }
)

test_that(
  "hits-method for subcorpus_bundle",
  {
    b <- corpus("GERMAPARLMINI") %>% split(s_attribute = "speaker")
    y <-  hits(b, query = "Integration")
    
    expect_identical(
      y@stat[partition == "Hermann Otto Solms"][["count"]],
      count(b[["Hermann Otto Solms"]], query = "Integration")[["count"]]
      )
    
  }
)


test_that(
  "merge-method for subcorpus_bundle",
  {
    a <- corpus("GERMAPARLMINI") %>% subset(interjection == "speech")
    b <- split(a, s_attribute = "speaker") %>% merge(b)
    expect_identical(a@cpos, b@cpos)
  }
)


test_that(
  "ngrams-method for subcorpus_bundle",
  {
    a <- corpus("GERMAPARLMINI") %>%
      subset(interjection == "speech") %>%
      subset(date == "2009-11-11") %>%
      split(s_attribute = "speaker") %>%
      ngrams(n = 2)
    
    b <- partition("GERMAPARLMINI", interjection = "speech", date = "2009-11-11") %>%
      partition_bundle(s_attribute = "speaker") %>%
      .[[names(a)]] %>%
      ngrams(n = 2)
    
    expect_identical(a, b)
  }
)

test_that(
  "split-method for subcorpus_bundle",
  {
    scb <- corpus("GERMAPARLMINI") %>%
      subset(interjection == "speech") %>%
      split(s_attribute = "date") %>%
      split(s_attribute = "speaker")
    
    y1 <- summary(scb)
    
    y2 <- partition("GERMAPARLMINI", interjection = "speech") %>%
      partition_bundle(s_attribute = "date") %>%
      partition_bundle(s_attribute = "speaker") %>%
      .[[names(scb)]] %>%
      summary()
    
    expect_identical(y1, y2)
  }
)

test_that(
  "summary-method for subcorpus_bundle",
  {
    y1 <- corpus("GERMAPARLMINI") %>% split(s_attribute = "date") %>% summary()
    y2 <- partition_bundle("GERMAPARLMINI", s_attribute = "date") %>% .[[ y1[["name"]] ]] %>% summary()
    expect_identical(y1, y2)
  }
)