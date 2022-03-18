library(polmineR)
use("polmineR")
testthat::context("cpos")

test_that(
  "cpos-method for corpus",
  {
    r <- corpus("REUTERS")
    expect_equal(nrow(cpos(r, query = "oil")), 78L)
    expect_equal(nrow(cpos(r, query = '"barrel.*"', cqp = TRUE)), 26L)
    expect_equal(cpos(r, query = "asdfasdfasdfasdf"), NULL)
    expect_equal(cpos(r, query = '"adfadfsaasdf.*"', cqp = TRUE), NULL)
  }
)

test_that(
  "cpos-method for corpus provided by character ID",
  {
    expect_equal(nrow(cpos("REUTERS", query = "oil")), 78L)
    expect_equal(nrow(cpos("REUTERS", query = '"barrel.*"', cqp = TRUE)), 26L)
    expect_equal(cpos("REUTERS", query = "asdfasdfasdfasdf"), NULL)
    expect_equal(cpos("REUTERS", query = '"adfadfsaasdf.*"', cqp = TRUE), NULL)
  }
)

test_that(
  "cpos-method for partition",
  {
    P <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
    expect_equal(nrow(cpos(P, query = "oil", p_attribute = "word")), 21L)
    expect_equal(nrow(cpos(P, query = '"barrel.*"', cqp = TRUE)), 7L)
    expect_equal(cpos(P, query = "asdfasdfasdfasdf", p_attribute = "word"), NULL)
    expect_equal(cpos(P, query = '"adfadfsaasdf.*"', cqp = TRUE), NULL)
  }
)

test_that(
  "cpos-method for subcorpus",
  {
    sc <- corpus("REUTERS") %>% subset(grepl("saudi-arabia", places))
    expect_equal(nrow(cpos(sc, query = "oil", p_attribute = "word")), 21L)
    expect_equal(nrow(cpos(sc, query = '"barrel.*"', cqp = TRUE)), 7L)
    expect_equal(cpos(sc, query = "asdfasdfasdfasdf", p_attribute = "word"), NULL)
    expect_equal(cpos(sc, query = '"adfadfsaasdf.*"', cqp = TRUE), NULL)
  }
)

test_that(
  "remove whitespace when looking up a token",
  {
    r <- corpus("REUTERS")
    expect_warning(cpos(r, query = " oil"))
    expect_warning(cpos(r, query = "oil "))
    expect_warning(cpos(r, query = "  oil "))
    expect_identical(nrow(cpos(r, query = "  oil ")), 78L)
  }
)
