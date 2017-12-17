library(polmineR)
use("polmineR")

testthat::context("count-method")

test_that("count using polmineR.Rcpp",{
  if (requireNamespace("polmineR.Rcpp", quietly = TRUE)){
    options("polmineR.Rcpp" = TRUE)
    dt <- count("REUTERS")
    expect_true(all(colnames(dt) %in% c("word", "word_id", "count")))
    expect_true(is.integer(dt[["count"]]))
    expect_true(is.integer(dt[["word_id"]]))
    expect_true(is.character(dt[["word"]]))
    expect_equal(sum(dt[["count"]]), 4050)
    expect_equal(dt[word == "barrel"][["count"]], 15)
  }
})

test_that("count using cwb-lexdecode", {
  if (getOption("polmineR.cwb-lexdecode") == TRUE){
    options("polmineR.Rcpp" = FALSE)
    dt <- count("REUTERS")
    expect_true(all(colnames(dt) %in% c("word", "word_id", "count")))
    expect_true(is.integer(dt[["count"]]))
    expect_true(is.integer(dt[["word_id"]]))
    expect_true(is.character(dt[["word"]]))
    expect_equal(sum(dt[["count"]]), 4050)
    expect_equal(dt[word == "barrel"][["count"]], 15)
  }
})

test_that("count using rcqp", {
  if (requireNamespace("rcqp", quietly = TRUE)){
    options("polmineR.Rcpp" = FALSE)
    options("polmineR.cwb-lexdecode" = FALSE)
    dt <- count("REUTERS")
    expect_true(all(colnames(dt) %in% c("word", "word_id", "count")))
    expect_true(is.integer(dt[["count"]]))
    expect_true(is.integer(dt[["word_id"]]))
    expect_true(is.character(dt[["word"]]))
    expect_equal(sum(dt[["count"]]), 4050)
    expect_equal(dt[word == "barrel"][["count"]], 15)
  }
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