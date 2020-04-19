library(polmineR)
testthat::context("decode")
use("polmineR")


test_that(
  "get size of corpus",
  expect_equal(size("GERMAPARLMINI"), 222201)
)


test_that(
  "size of corpus, split by one s-attribute", {
    y <- size("GERMAPARLMINI", sAttribute = "date")
    expect_equal(sum(y[["size"]]), size("GERMAPARLMINI"))
    expect_equal(nrow(y), length(sAttributes("GERMAPARLMINI", "date")))
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
    y <- size("GERMAPARLMINI", sAttribute = c("date", "party"))
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
  "size of partition", {
    P <- partition("GERMAPARLMINI", date = "2009-11-11")
    expect_equal(size(P), 117614)
    expect_equal(sum(size(P, sAttribute = "speaker")[["size"]]), size(P))
    expect_equal(sum(size(P, sAttribute = c("speaker", "party"))[["size"]]), size(P))
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

