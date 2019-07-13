library(polmineR)
use("polmineR")

testthat::context("count-method")

test_that("count",{
  dt <- count("REUTERS")
  expect_true(all(colnames(dt) %in% c("word", "word_id", "count")))
  expect_true(is.integer(dt[["count"]]))
  expect_true(is.integer(dt[["word_id"]]))
  expect_true(is.character(dt[["word"]]))
  expect_equal(sum(dt[["count"]]), 4050)
  expect_equal(dt@stat[word == "barrel"][["count"]], 15)
})



reuters <- partition("REUTERS", list(id = ".*"), regex = TRUE)

test_that("count (one query)", {
  expect_equal(count(reuters, query = "is")[["count"]], 25)
})

test_that("count (multiple queries)", {
  expect_equal(
    count(reuters, c("is", "this", "real"))[["count"]],
    c(25, 7, 3)
  )
})


test_that("count - breakdown", {
  y <- count("REUTERS", query = '"remain.*"', breakdown = TRUE)
  expect_equal(sum(y[["count"]]), 5)
})


test_that(
  "count over partition_bundle",
  {
    cnt_int_total <- corpus("GERMAPARLMINI") %>% count(query = "Integration")
    sp_bundle <- corpus("GERMAPARLMINI") %>%
      as.speeches(s_attribute_name = "speaker")
    cnt_int_pb <- count(sp_bundle, query = "Integration") %>%
      subset(Integration > 0)
    expect_equal(sum(cnt_int_pb[["TOTAL"]]), cnt_int_total[["count"]])
    for (i in 1L:nrow(cnt_int_pb)){
      exp <- count(sp_bundle[[cnt_int_pb[["partition"]][i]]], query = "Integration")[["count"]]
      expect_equal(exp, cnt_int_pb[["TOTAL"]][i])
    }
  })