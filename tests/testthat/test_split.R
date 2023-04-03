library(polmineR)
testthat::context("split")
use("polmineR")


test_that(
  "split up corpus",
  {
    x <- partition("GERMAPARLMINI", date = "2009-11-11", speaker = "Gerda Hasselfeldt")
    y <- split(x, gap = 500)
    expect_identical(is(y)[1], "partition_bundle")
    expect_identical(length(y), 15L)
    expect_identical(size(merge(y)), size(x))
    
    y2 <- merge(y)
    expect_identical(as.integer(merge(y)@cpos), as.integer(x@cpos))
    expect_identical(as.vector(merge(y)@cpos), as.vector(x@cpos))
  }
)

test_that(
  "different order, same result",
  {
    pp1 <- corpus("GERMAPARLMINI") %>%
      subset(protocol_date == "2009-11-10") %>%
      split(s_attribute = "speaker")
    
    pp2 <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-10") %>%
      split(s_attribute = "speaker")
    
    pp3 <- corpus("GERMAPARLMINI") %>%
      split(s_attribute = "speaker") %>%
      .[["Angela Dorothea Merkel"]] %>%
      subset(date == "2009-11-10")
    
    dimnames(pp3@cpos) <- NULL
    
    expect_identical(
      pp1[["Angela Dorothea Merkel"]]@cpos,
      pp2[["Angela Dorothea Merkel"]]@cpos
    )
    
    expect_identical(
      pp1[["Angela Dorothea Merkel"]]@cpos,
      pp3@cpos
    )
  }
)

