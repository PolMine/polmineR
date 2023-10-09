library(polmineR)
use("polmineR")

testthat::context("s_attributes-method")

test_that(
  "s_attributes for corpus, without specification of s_attribute",
  {
    s_attrs <- s_attributes("GERMAPARLMINI")
    expect_equal(length(s_attrs), 7L)
    expect_equal(is.character(s_attrs), TRUE)
    expect_equal(all(s_attrs %in% c("protocol_lp", "protocol_date", "interjection", "date", "party", "speaker", "role")), TRUE)
    
    s_attrs <- corpus("GERMAPARLMINI") %>%
      s_attributes()
    expect_equal(length(s_attrs), 7L)
    expect_equal(is.character(s_attrs), TRUE)
    expect_equal(all(s_attrs %in% c("protocol_lp", "protocol_date", "interjection", "date", "party", "speaker", "role")), TRUE)
  }
)

test_that(
  "multiple s-attributes",{
    s_attr_unique <- s_attributes("GERMAPARLMINI", s_attribute = c("date", "speaker"))
    s_attr_all <- s_attributes("GERMAPARLMINI", s_attribute = c("date", "speaker"), unique = FALSE)
    
    expect_equal(class(s_attr_unique)[1], "data.table")
    expect_equal(class(s_attr_all)[1], "data.table")
    
    expect_identical(ncol(s_attr_unique), 2L)
    expect_identical(ncol(s_attr_all), 2L)
    
    expect_identical(nrow(s_attr_unique), 155L)
    expect_identical(nrow(s_attr_all), 4810L)
    
    s_attr_sub_unique <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-11") %>%
      s_attributes(s_attribute = c("date", "speaker"))
    s_attr_sub_all <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-11") %>%
      s_attributes(s_attribute = c("date", "speaker"), unique = FALSE)
    
    expect_equal(class(s_attr_sub_unique)[1], "data.table")
    expect_equal(class(s_attr_sub_all)[1], "data.table")
    
    expect_identical(ncol(s_attr_sub_unique), 2L)
    expect_identical(ncol(s_attr_sub_all), 2L)
    
    expect_identical(
      all(s_attr_unique[date == "2009-11-11"][["speaker"]] %in% s_attr_sub_unique[["speaker"]]),
      TRUE
    )
    expect_identical(
      all(s_attr_all[date == "2009-11-11"][["speaker"]] %in% s_attr_sub_all[["speaker"]]),
      TRUE
    )
    
    # If a subcorpus has only one region, the resulting data.table may be mixed up.
    x <- corpus("REUTERS") %>% subset(id == "237")
    s_attr_dt <- s_attributes(x, s_attribute = c("id", "places", "language"), unique = FALSE)
    expect_equal(nrow(s_attr_dt), nrow(x@cpos))
  }
)


test_that(
  "get s-attributes in a call or a quosure",
  {
    expect_equal(
      s_attributes(quote(grep("Merkel", speaker)), corpus = "GERMAPARLMINI"),
      setNames("speaker", "character")
    )
    expect_equal(
      s_attributes(quote(speaker == "Angela Merkel"), corpus = "GERMAPARLMINI"),
      setNames("speaker", "character")
    )
    expect_equal(
      s_attributes(
        quote(speaker == "Angela Merkel" & date == "2009-10-28"),
        corpus = "GERMAPARLMINI"
      ),
      c(character = "speaker", character = "date")
    )
    
    expect_equal(
      s_attributes(
        rlang::new_quosure(quote(grep("Merkel", speaker))),
        corpus = "GERMAPARLMINI"
      ),
      setNames("speaker", "character")
    )
    
  }
)
