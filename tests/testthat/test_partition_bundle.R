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
    
    scb <- split(x = corpus("GERMAPARLMINI"), s_attribute = "speaker")
    expect_equal(length(pb), length(scb))
    
    expect_equal(lapply(scb@objects, size), lapply(pb@objects, size))
    foo <- lapply(seq_along(scb@objects), function(i){ expect_equal(pb[[i]]@cpos, scb[[i]]@cpos); invisible(NULL) })
    expect_equal(names(pb), names(scb))
    expect_equal(as(pb, "subcorpus_bundle"), scb)
    
    
    p <- partition("GERMAPARLMINI", interjection = "speech")
    pb <- partition_bundle(p, s_attribute = "speaker")
    s <- summary(pb)
    expect_equal(s[grep("Schwall-", s[["name"]]), ][["size"]], 1119L)
    
    sc <- subset(corpus("GERMAPARLMINI"), interjection == "speech")
    scb <- split(sc, s_attribute = "speaker")

    expect_equal(length(scb), length(pb))
    
    expect_equal(all(names(scb) %in% names(pb)), TRUE)
    pb_2 <- pb[[names(scb)]] # necessary, because order may be different
    pb@objects <- pb_2@objects
    expect_equal(names(scb), names(pb))
    expect_equal(lapply(scb@objects, size), lapply(pb@objects, size))
    foo <- lapply(
      seq_along(scb@objects),
      function(i) expect_equal(pb[[i]]@cpos, scb[[i]]@cpos)
    )
    expect_equal(names(pb), names(scb))
    y <- as(pb, "subcorpus_bundle")
    expect_equal(y, scb)
  }
)

