library(polmineR)

testthat::context("get_token_stream")

test_that(
  "get_token_stream",
  {
    fulltext <- get_token_stream("REUTERS", p_attribute = "word")
    expect_identical(length(fulltext), 4050L)
    expect_identical(
      head(fulltext),
      c("Diamond", "Shamrock", "Corp", "said", "that", "effective")
    )
    
    y <- get_token_stream(0L:9L, corpus = "GERMAPARLMINI", p_attribute = "word")
    expect_identical(
      y,
      c("Guten", "Morgen", ",", "meine", "sehr", "verehrten", "Damen", "und", "Herren", "!")
    )
    
  }
)

