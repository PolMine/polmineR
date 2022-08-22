library(polmineR)
testthat::context("decode")
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")


test_that(
  "get size of corpus",
  expect_equal(size("GERMAPARLMINI"), 222201)
)


test_that(
  "size of corpus, split by one s-attribute", {
    y <- size("GERMAPARLMINI", s_attribute = "date")
    expect_equal(sum(y[["size"]]), size("GERMAPARLMINI"))
    expect_equal(nrow(y), length(s_attributes("GERMAPARLMINI", "date")))
    expect_equal(y[["size"]][1:3], c(9341, 2793, 68316))
    
    y <- corpus("GERMAPARLMINI") %>% size(s_attribute = "date")
    expect_equal(sum(y[["size"]]), size("GERMAPARLMINI"))
    expect_equal(nrow(y), length(s_attributes("GERMAPARLMINI", "date")))
    expect_equal(y[["size"]][1:3], c(9341, 2793, 68316))
  }
)

test_that(
  "size of corpus, two s-attributes", {
    
    # corpus stated by ID
    y <- size("GERMAPARLMINI", s_attribute = c("date", "party"))
    expect_equal(length(unique(y[["party"]])), 6)
    expect_equal(sum(y[["size"]]), size("GERMAPARLMINI"))
    expect_equal(y[["size"]][1:3], c(17,71,25))
    
    # corpus object passed in
    y <- corpus("GERMAPARLMINI") %>% size(s_attribute = c("date", "party"))
    expect_equal(length(unique(y[["party"]])), 6)
    expect_equal(sum(y[["size"]]), size("GERMAPARLMINI"))
    expect_equal(y[["size"]][1:3], c(17,71,25))

  }
)

test_that(
  "size of partition / subcorpus",
  {
    P <- partition("GERMAPARLMINI", date = "2009-11-11")
    expect_equal(size(P), 117614)
    expect_equal(sum(size(P, s_attribute = "speaker")[["size"]]), size(P))
    expect_equal(sum(size(P, s_attribute = c("speaker", "party"))[["size"]]), size(P))
    
    coi <- corpus("GERMAPARLMINI") %>%
      subset(grepl("2009-11-.*", protocol_date))
    
    s <- size(coi, s_attribute = "speaker", verbose = FALSE)
    a <- size(coi, s_attribute = c("speaker", "date"), verbose = FALSE)
    b <- size(coi, s_attribute = c("speaker", "protocol_date"), verbose = FALSE)
    expect_identical(sum(a$size), sum(b$size))
    # expect_identical(sum(a$size), size("GERMAPARLMINI"))
  }
)

test_that(
  "size for partition_bundle / subcorpus_bundle",
  {
    reuters_articles <- corpus("REUTERS") %>% split(s_attribute = "id")
    # This check is somewhat redundant but good to be on the safe side
    to_be_checked <- size(reuters_articles)[["size"]]
    expect_identical(length(to_be_checked), length(reuters_articles))
    expect_identical(
      unname(sapply(reuters_articles, size)), # Get size for each subcorpus individually
      size(reuters_articles)[["size"]] # bulk processing of size,partition_bundle
    )
  }
)

