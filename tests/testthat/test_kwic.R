library(polmineR)
use("polmineR")
testthat::context("kwic")

test_that(
  "kwic-method for corpus",
  {
    expect_equal(
      nrow(kwic("REUTERS", query = "oil", pAttribute = "word")@stat),
      78L
      )
    
    expect_equal(
      nrow(kwic("REUTERS", query = '"barrel.*"', pAttribute = "word")@stat),
      26L
      )

    expect_equal(
      kwic("REUTERS", query = "asdfasdf", pAttribute = "word"),
      NULL
      )
    
    expect_equal(
      kwic("REUTERS", query = '"asdfasdfasdfasd.*"', cqp = TRUE),
      NULL
    )
  }
)

test_that(
  "kwic-method for partition",
  {
    P <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
    
    expect_equal(
      nrow(kwic(P, query = "oil", pAttribute = "word")@stat),
      21L
    )
    
    expect_equal(
      nrow(kwic(P, query = '"barrel.*"', cqp = TRUE, pAttribute = "word")@stat),
      7L
      )

    expect_equal(
      kwic(P, query = "asdfasdf", pAttribute = "word"),
      NULL
      )
    
    expect_equal(
      kwic(P, query = '"asdfasdfasdfasd.*"', cqp = TRUE, pAttribute = "word"),
      NULL
    )
  }
)

test_that(
  "as.character-method for kwic objects",
  {
    oil <- corpus("REUTERS") %>% kwic(query = "oil")
    str <- as.character(oil, fmt = NULL)
    expect_equal(length(str), 78L)
    expect_equal(str[1], "its contract prices for crude oil by 1.50 dlrs a barrel")
    expect_equal(
      as.character(oil)[1],
      "its contract prices for crude <i>oil</i> by 1.50 dlrs a barrel"
    )
    expect_equal(
      as.character(corpus("REUTERS") %>% kwic(query = "oil"), fmt = "<b>%s</b>")[1],
      "its contract prices for crude <b>oil</b> by 1.50 dlrs a barrel"
    )
  }
)

test_that(
  "indexing kwic objects",
  {
    k <- corpus("REUTERS") %>% kwic(query = "oil")
    k2 <- k[1:5]
    expect_identical(unique(k2@cpos[["match_id"]]), k2@stat[["match_id"]])
  }
)

test_that(
  "subsetting kwic objects",
  {
    oil <- corpus("REUTERS") %>% kwic(query = "oil") %>% subset(grepl("prices", right))
    expect_identical(unique(oil@cpos[["match_id"]]), oil@stat[["match_id"]])
    
    int_spd <- corpus("GERMAPARLMINI") %>%
      kwic(query = "Integration") %>%
      enrich(s_attribute = "party") %>%
      subset(grepl("SPD", party))
    expect_identical(unique(int_spd@stat[["party"]]), "SPD")
  }
)

test_that(
  "as.data.frame for kwic-method",
  {
    int <- corpus("GERMAPARLMINI") %>%
      kwic(query = "Integration") %>%
      enrich(s_attributes = c("date", "speaker", "party")) %>%
      as.data.frame()
    expect_equal(int[[1]][1], "2009-10-27<br/>Heinz Riesenhuber<br/>NA")
    
  }
)

test_that(
  "as.DocumentTermMatrix for kwic-class-object",
  {
    oil <- kwic("REUTERS", query = "oil")
    dtm <- as.DocumentTermMatrix(oil, p_attribute = "word")
    expect_equal(
      slam::col_sums(dtm)[["prices"]],
      nrow(oil@cpos[word == "prices" & direction != 0L])
    )
  }
)

test_that(
  "kwic: NULL object if positivelist removes all matches",
  {
    k <- corpus("GERMAPARLMINI") %>% kwic(query = 'Integration', cqp = FALSE, positivelist = "Messer")
    expect_equal(is.null(k), TRUE)
  }
)

test_that(
  "kwic: Apply kwic on partition_bundle",
  {
    sp <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-10") %>%
      split(s_attribute = "speaker")
    kwic_table <- kwic(sp, query = "Integration") %>% slot("stat")
    
    # The idea of the test is that the number of concordences per subcorpus 
    # needs to be identical with the result of a count over the partition_bundle
    dt <- kwic_table[, .N, by = "subcorpus_name"]
    data.table::setorderv(dt, cols = "N", order = -1L)
    cnt <- count(sp, query = "Integration", s_attributes = "speaker", progress = FALSE)
    cnt <- cnt[TOTAL > 0L]
    setorderv(cnt, cols = "TOTAL", order = -1L)
    expect_equal(dt[["subcorpus_name"]], cnt[["partition"]])
    expect_equal(dt[["N"]], cnt[["TOTAL"]])
  }
)
