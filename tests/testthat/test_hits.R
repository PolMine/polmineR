library(polmineR)
use(pkg = "RcppCWB", corpus = "REUTERS")
testthat::context("hits")

test_that(
  "hits method",
  {
    y <- hits("REUTERS", query = "oil")
    expect_equal(as.data.table(y)[["count"]], count("REUTERS", query = "oil")[["count"]])
    
    y <- hits("REUTERS", query = "oil", s_attribute = "places", freq = TRUE)
    expect_equal(y@stat[places == "argentina"][["count"]], 1L)

    p <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
    y <- hits(p, query = "oil")
    expect_equal(as.data.table(y)[["count"]], count(p, query = "oil")[["count"]])
    
    y <- hits(p, query = "oil", s_attribute = "id")
    expect_equal(y@stat[id == "242"][["count"]], count(partition("REUTERS", id = "242"), query = "oil")[["count"]])
  }
)

test_that(
  "hits for nested scenario",
  {
    skip_if_not(use("GermaParl2"))

    # we want to ensure that the order is independent from the order of 
    # subsetting operations
    
    hits1 <- corpus("GERMAPARL2MINI") |>
      subset(p_type == "speech") %>% 
      subset(speaker_party %in% c("CDU", "CSU", "SPD")) |>
      split(s_attribute = "speaker_party", verbose = FALSE) %>% 
      hits(query = tm::stopwords("de")[1:10], cqp = FALSE, verbose = FALSE) %>% 
      as.data.table()
    
    hits2 <- corpus("GERMAPARL2MINI") |>
      subset(speaker_party %in% c("CDU", "CSU", "SPD")) |>
      split(s_attribute = "speaker_party") %>% 
      subset(p_type == "speech", verbose = FALSE) %>% 
      hits(query = tm::stopwords("de")[1:10], cqp = FALSE, verbose = FALSE) %>% 
      as.data.table()
    
    testthat::expect_identical(hits1, hits2)
    
    hits3 <- corpus("GERMAPARL2MINI") |>
      subset(speaker_party == "CDU") |>
      subset(p_type == "speech", verbose = FALSE) %>% 
      hits(query = tm::stopwords("de")[1:10], cqp = FALSE, verbose = FALSE) %>% 
      as.data.table()
    
    hits4 <- corpus("GERMAPARL2MINI") |>
      subset(p_type == "speech") %>% 
      subset(speaker_party == "CDU") |>
      hits(query = tm::stopwords("de")[1:10], cqp = FALSE, verbose = FALSE) %>% 
      as.data.table()
    
    testthat::expect_identical(hits3, hits4)
    
    testthat::expect_identical(
      hits2[partition == "CDU"][, c("query", "count")],
      hits3[count > 0][,c("query", "count")]
    )
    
  }
)

test_that(
  "test arg decode of hits()",
  {
    s_attrs <- c("id", "places", "language")
    
    corpus("REUTERS") %>% 
      hits(query = "oil", s_attribute = s_attrs, decode = TRUE) %>% 
      as.data.table() %>% 
      .[, s_attrs, with = FALSE] %>% 
      sapply(typeof) %>% 
      unname() %>% 
      unique() %>% 
      expect_identical("character")
    
    corpus("REUTERS") %>% 
      hits(query = "oil", s_attribute = s_attrs, decode = FALSE) %>% 
      as.data.table() %>% 
      .[, s_attrs, with = FALSE] %>% 
      sapply(typeof) %>% 
      unname() %>% 
      unique() %>% 
      expect_identical("integer")
    

    corpus("REUTERS") %>% 
      hits(query = "oil", s_attribute = s_attrs, decode = c(FALSE, TRUE, TRUE)) %>% 
      as.data.table() %>% 
      .[, s_attrs, with = FALSE] %>% 
      sapply(typeof) %>% 
      unname() %>%
      expect_identical(c("integer", "character", "character"))
  }
)
