library(polmineR)
use("polmineR")
testthat::context("as.data.table")

test_that(
  "as.data.table.bundle",
  {
    skip_on_cran()
    
    use("polmineR")
    pb <- partition_bundle("REUTERS", s_attribute = "id")
    coocs <- cooccurrences(pb, query = "Saudi", cqp = FALSE, verbose = FALSE, progress = FALSE)
    dt <- as.data.table(coocs, col = "ll")
    
    expect_warning(as.data.table(coocs, col = "ll", keep.rownames = FALSE))
    expect_warning(as.data.table(coocs, col = "ll", foo = "bar"))
    
    m <- as.matrix(dt[, 2:ncol(dt)], rownames = dt[["token"]])
    
    token <- "Minister"
    dt <- corpus("REUTERS") %>%
      subset(id == "248") %>%
      cooccurrences("Saudi") %>%
      as.data.table()
    
    expect_identical(m[token, "248"], dt[word == token][["ll"]])
  }
)


test_that(
  "as.data.table",
  {
    reuters_subset <- corpus("REUTERS") %>%
      subset(grep("saudi-arabia", places))
    
    count_obj <- count(reuters_subset, p_attribute = "word")
    count_dt <- as.data.table(count_obj)
    
    expect_identical(size(reuters_subset), sum(count_dt[["count"]]))
    expect_warning(as.data.table(count_obj, foo = "bar"))
  }
)
