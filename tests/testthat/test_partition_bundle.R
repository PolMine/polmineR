library(polmineR)
library(data.table)

testthat::context("partition_bundle")

test_that(
  "partition_bundle",
  {
    pb <- partition_bundle("GERMAPARLMINI", s_attribute = "speaker")
    s <- summary(pb)
    expect_equal(s[grep("Schwall-", s[["name"]]), ][["size"]], 1230L)
    expect_equal(sum(s[["size"]]), size("GERMAPARLMINI"))
    
    p <- partition("GERMAPARLMINI", interjection = "speech")
    pb <- partition_bundle(p, s_attribute = "speaker")
    s <- summary(pb)
    expect_equal(s[grep("Schwall-", s[["name"]]), ][["size"]], 1119L)
  }
)

