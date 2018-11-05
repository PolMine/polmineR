library(polmineR)
use("polmineR")
testthat::context("cooccurrences")

test_that(
  "cooccurrences-method for corpus",
  {
    expect_equal(
      cooccurrences("REUTERS", query = "oil", pAttribute = "word")@stat[["word"]][1:5],
      c("prices", "crude", "world", "markets", "industry")
    )
    
    expect_equal(
      cooccurrences("REUTERS", query = '"barrel.*"', pAttribute = "word")@stat[["word"]][1:5],
      c("dlrs", "mln", "a", "18", "per")
    )
    
    expect_equal(
      cooccurrences("REUTERS", query = "asdfasdf", pAttribute = "word"),
      NULL
    )
    
    expect_equal(
      cooccurrences("REUTERS", query = '"asdfasdfasdfasd.*"', cqp = TRUE),
      NULL
    )
  }
)

test_that(
  "cooccurrences-method for partition",
  {
    P <- partition("REUTERS", places = "saudi-arabia", regex = TRUE)
    
    expect_equal(
      cooccurrences(P, query = "oil", pAttribute = "word")@stat[["word"]][1:5],
      c("prices", "each", "market", "other", "crude")
    )
    
    expect_equal(
      cooccurrences(P, query = '"barrel.*"', cqp = TRUE, pAttribute = "word")@stat[["word"]][1:5],
      c("a", "dlrs", "18", "day","per")
    )
    
    expect_equal(
      cooccurrences(P, query = "asdfasdf", pAttribute = "word"),
      NULL
    )
    
    expect_equal(
      cooccurrences(P, query = '"asdfasdfasdfasd.*"', cqp = TRUE, pAttribute = "word"),
      NULL
    )
  }
)


test_that(
  "Identity of cooccurrences and Cooccurrences",
  {
    stopwords <- unname(unlist(noise(terms("REUTERS", p_attribute = "word"), stopwordsLanguage = "en")))
    r <- Cooccurrences("REUTERS", p_attribute = "word", left = 5L, right = 5L, stoplist = stopwords)
    stm <- as.simple_triplet_matrix(r)
    
    decode(r)
    spm <- as.sparseMatrix(r)
    
    coocs <- list(
      c("oil", "prices"),
      c("Saudi", "Arabia"),
      c("Sheikh", "Ali"),
      c("barrel", "dlrs")
    )
    
    lapply(
      coocs,
      function(tokens){
        print(coocs)
        a2b <- as.integer(as.matrix(stm[tokens[1], tokens[2]]))
        b2a <- as.integer(as.matrix(stm[tokens[2], tokens[1]]))
        expect_equal(a2b, b2a)
        
        expect_equal(spm[tokens[1], tokens[2]], spm[tokens[2], tokens[1]])
        
        a2b_cqp <- count("REUTERS", sprintf('"%s" []{0,4} "%s"', tokens[1], tokens[2]), cqp = TRUE)
        b2a_cqp <- count("REUTERS", sprintf('"%s" []{0,4} "%s"', tokens[2], tokens[1]), cqp = TRUE)
        expect_equal(a2b_cqp[["count"]] + b2a_cqp[["count"]], a2b)
        
        expect_equal(a2b_cqp[["count"]] + b2a_cqp[["count"]], spm[tokens[1], tokens[2]])
        
        expect_equal(
          a2b,
          cooccurrences("REUTERS", query = tokens[1])@stat[word == tokens[2]][["count_window"]]
        )
        
        NULL
    })
    
    
    
    ll(r)
    decode(r)

    a <- data.table::as.data.table(cooccurrences(r, query = "oil"))
    b <- data.table::as.data.table(cooccurrences("REUTERS", query = "oil"))[!word %in% stopwords]

    expect_equal(a[["word"]][1:14], b[["word"]][1:14])
    expect_equal(a[["count_partition"]], b[["count_partition"]])
    expect_equal(a[["count_window"]], b[["count_window"]])
    expect_equal(a[["exp_window"]], b[["exp_window"]])
    expect_equal(a[["exp_partition"]], b[["exp_partition"]])
    expect_equal(a[["ll"]], b[["ll"]])
  }
)


