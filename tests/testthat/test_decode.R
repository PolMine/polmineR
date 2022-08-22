library(polmineR)
testthat::context("decode")
use("polmineR")


test_that(
  "decode entire corpus",
  {
    dt <- decode("GERMAPARLMINI", to = "data.table")
    expect_equal(ncol(dt), 8L)
    expect_equal(nrow(dt), 222201L)
    expect_equal(dt[["word"]][1:6], c("Guten", "Morgen", ",", "meine", "sehr", "verehrten"))
    expect_equal(length(unique(dt[["date"]])), 5L)
  }
)

test_that(
  "decode corpus selectively",
  {
    dt <- corpus("GERMAPARLMINI") %>% 
      decode(to = "data.table", s_attributes = "party", p_attributes = "word")
    expect_equal(ncol(dt), 3L)
    expect_equal(nrow(dt), 222201L)
    expect_equal(dt[["word"]][1:6], c("Guten", "Morgen", ",", "meine", "sehr", "verehrten"))
    expect_equal(length(unique(dt[["party"]])), 6L)
  }
)

test_that(
  "decode corpus without s-attributes",
  {
    dt <- corpus("GERMAPARLMINI") %>% 
      decode(to = "data.table", s_attributes = character(), p_attributes = "word")
    expect_equal(ncol(dt), 2L)
    expect_equal(nrow(dt), 222201L)
    expect_equal(dt[["word"]][1:6], c("Guten", "Morgen", ",", "meine", "sehr", "verehrten"))
  }
)


test_that(
  "decode subcorpus without any s-attributes",
  {
    dt <- corpus("GERMAPARLMINI") %>% 
      subset(party == "SPD") %>%
      decode(to = "data.table", p_attributes = "word", s_attributes = character())
    expect_equal(ncol(dt), 2L)
    expect_equal(nrow(dt), 47302L)
    expect_equal(dt[["word"]][1:8], c("Ja", ",", "ich", "nehme", "die", "Wahl", "an", "."))
  }
)

test_that(
  "decode token ids",
  {
    gparl <- corpus("GERMAPARLMINI")
    ids <- RcppCWB::cl_cpos2id(
      corpus = gparl@corpus, registry = gparl@registry_dir,
      p_attribute = "word",
      cpos = 0L:(gparl@size - 1L)
    )
    a <- decode(ids, corpus = gparl, p_attribute = "word", boost = FALSE)
    b <- decode(ids, corpus = gparl, p_attribute = "word", boost = TRUE)
    expect_identical(a,b)
  }
)
