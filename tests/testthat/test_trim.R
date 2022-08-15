library(polmineR)
testthat::context("enrich")
use("RcppCWB")


test_that(
  "trim context",
  {
    oil <- context("REUTERS", query = "oil")
    oil_no_crude <- polmineR::trim(oil, stoplist = "crude")
    expect_identical(length(oil_no_crude), 78L - 13L)
    expect_identical(
      nrow(oil_no_crude@cpos[position != 0L]),
      sum(oil_no_crude@stat[["count_coi"]])
    )
    
    oil <- polmineR::context("REUTERS", query = "oil")
    oil_just_crude <- polmineR::trim(oil, positivelist = "crude")
    expect_identical(length(oil_just_crude), 78L - 65L)
    expect_identical(
      nrow(oil_just_crude@cpos[position != 0L]),
      sum(oil_just_crude@stat[["count_coi"]])
    )
    
    # apply filtering function
    oil <- polmineR::context("REUTERS", "oil")
    .fn <- function(DT) if (DT[position == -1L][["word_id"]] == 14) DT else NULL
    oil_min <- polmineR::trim(oil, fn = .fn)
    expect_identical(length(oil_min), count("REUTERS", '"crude" "oil"')$count)
    
    oil2 <- enrich(oil, p_attribute = "word", decode = TRUE)
    .fn <- function(DT) if (DT[position == -1L][["word"]] == "crude") DT else NULL
    oil_min <- polmineR::trim(oil2, fn = .fn)
    expect_identical(
      length(oil_min),
      count("REUTERS", '"crude" "oil"')$count
    )
    
  }
)

