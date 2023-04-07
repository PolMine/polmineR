library(polmineR)
testthat::context("enrich")
use("polmineR")
use("RcppCWB")

test_that(
  "enrich partition",
  {
    x <- partition("GERMAPARLMINI", date = "2009-11-11", speaker = "Gerda Hasselfeldt")
    y <- enrich(x, p_attribute = "word")
    cnt <- count(x, p_attribute = "word")
    expect_identical(y@stat, cnt@stat)
  }
)


test_that(
  "enrich partition_bundle",
  {
    pb <- corpus("REUTERS") %>%
      partition_bundle(s_attribute = "id") %>%
      enrich(p_attribute = "word")
    
    p <- corpus("REUTERS") %>%
      partition(id = s_attributes(pb[[1]], "id")) %>%
      enrich(p_attribute = "word")
    
    dt_pb <- as.data.table(pb[[1]])
    dt_p <- as.data.table(p)
    
    expect_identical(colnames(dt_pb), colnames(dt_p))
    
    for (x in colnames(dt_pb)) expect_identical(dt_pb[[x]], dt_p[[x]])
  }
)

test_that(
  "enrich subcorpus_bundle",
  {
    # The subcorpus class does not have slot 'stat' (for the time being),
    # so enriching a subcorpus_bundle requires coercion to partition_bundle.
    # This test checks that this coercion is performed.
    
    pb <- corpus("GERMAPARLMINI") %>%
      as.speeches(s_attribute_date = "date", s_attribute_name = "speaker") %>%
      enrich(p_attribute = "word")
    
    expect_identical(is(pb)[1], "partition_bundle")
    expect_identical(unique(sapply(pb@objects, "class")), "plpr_partition")
  }
)
