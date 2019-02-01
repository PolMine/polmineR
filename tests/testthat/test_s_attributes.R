library(polmineR)
use("polmineR")

testthat::context("s_attributes-method")

test_that(
  "s_attributes for corpus, without specification of s_attribute",
  {
    s_attrs <- s_attributes("GERMAPARLMINI")
    expect_equal(length(s_attrs), 4L)
    expect_equal(is.character(s_attrs), TRUE)
    expect_equal(all(s_attrs %in% c("interjection", "date", "party", "speaker")), TRUE)
    
    s_attrs <- s_attributes(corpus("GERMAPARLMINI"))
    expect_equal(length(s_attrs), 4L)
    expect_equal(is.character(s_attrs), TRUE)
    expect_equal(all(s_attrs %in% c("interjection", "date", "party", "speaker")), TRUE)
    
  }
)


test_that(
  "get s-attributes in a call",
  {
    expect_equal(
      s_attributes(quote(grep("Merkel", speaker)), corpus = "GERMAPARLMINI"),
      "speaker"
    )
    expect_equal(
      s_attributes(quote(speaker == "Angela Merkel"), corpus = "GERMAPARLMINI"),
      "speaker"
    )
    expect_equal(
      s_attributes(
        quote(speaker == "Angela Merkel" & date == "2009-10-28"),
        corpus = "GERMAPARLMINI"
      ),
      c("speaker", "date")
    )
  }
)
