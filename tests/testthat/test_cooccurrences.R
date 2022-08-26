library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("cooccurrences")

test_that(
  "cooccurrences-method for corpus",
  {
    y <- cooccurrences("REUTERS", query = "oil", p_attribute = "word")
    expect_equal(subset(y, !is.na(ll))[["word"]][1:4], c("prices", "crude", "industry", "recent"))
    
    y <- cooccurrences("REUTERS", query = '"barrel.*"', p_attribute = "word")
    expect_equal(subset(y, !is.na(ll))[["word"]][1:5], c("dlrs", "mln", "a", "reserve", "brings"))
    
    # handle more than one p-attribute
    p_attrs <- c("word", "pos")
    dt <- corpus("GERMAPARLMINI") %>%
      cooccurrences(query = "Arbeit", p_attribute = p_attrs) %>%
      format()
    expect_true(all(p_attrs %in% colnames(dt)))

    expect_equal(
      cooccurrences("REUTERS", query = "asdfasdf", p_attribute = "word"),
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
    
    y <- cooccurrences(P, query = "oil", p_attribute = "word")
    expect_equal(subset(y, !is.na(ll))[["word"]][1:5], c("prices", "below", "its", "crude", "market"))
    
    y <- cooccurrences(P, query = '"barrel.*"', cqp = TRUE, p_attribute = "word")
    expect_equal(subset(y, is.na(ll))[["word"]][1:5], c("10", "17.52", "18", "1986","3.5"))
    
    expect_equal(
      cooccurrences(P, query = "asdfasdf", p_attribute = "word"),
      NULL
    )
    
    expect_equal(
      cooccurrences(P, query = '"asdfasdfasdfasd.*"', cqp = TRUE, p_attribute = "word"),
      NULL
    )
  }
)


test_that(
  "Check log-likelihood formula",
  {
    cooc <- cooccurrences("GERMAPARLMINI", query = "Integration")
    cooc_dt <- cooc@stat[!is.na(ll)]
    data.table::setorderv(cooc_dt, cols = "ll", order = -1L)
    
    for (i in seq.int(from = 1L, to = 10L)){
      
      o11 <- cooc_dt[["count_coi"]][i]
      o12 <- cooc_dt[["count_ref"]][i]
      o21 <- cooc@size_coi - o11
      o22 <- cooc@size_ref - o12

      N <- o21 + o22 + o11 + o12
      
      e11 <- (o11 + o21) * ((o11 + o12) / N)
      e12 <- (o12 + o22) * ((o11 + o12) / N)
      e21 <- (o11 + o21) * ((o21 + o22) / N)
      e22 <- (o12 + o22) * ((o21 + o22) / N)
      
      ll <- 2 * (o11*log(o11/e11) + o12*log(o12/e12) + o21*log(o21/e21) + o22*log(o22/e22) )

      expect_identical(round(cooc_dt[["ll"]][i], 3), round(ll, 3))
    }
  }
)





test_that(
  "Identity of cooccurrences and Cooccurrences",
  {
    testthat::skip_on_cran()
    rm <- noise(
      terms("REUTERS", p_attribute = "word"),
      specialChars = NULL, minNchar = 2L, stopwordsLanguage = "en"
    )
    stopwords <- unname(unlist(rm))
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
          cooccurrences("REUTERS", query = tokens[1])@stat[word == tokens[2]][["count_coi"]]
        )

        NULL
    })



    ll(r)
    decode(r)

    a <- data.table::as.data.table(cooccurrences(r, query = "oil"))
    a <- a[!is.na(ll)][!is.nan(ll)]
    b <- data.table::as.data.table(cooccurrences("REUTERS", query = "oil"))[!word %in% stopwords]
    b <- b[!is.na(ll)][!is.nan(ll)]
    
    library(data.table)
    setkeyv(a, cols = "word")
    setkeyv(b, cols = "word")
    m <- a[b]

    expect_equal(m[["count_coi"]], m[["i.count_coi"]])
    expect_equal(m[["obs_ref"]], m[["i.count_ref"]])
    expect_equal(m[["exp_coi"]], m[["i.exp_coi"]])
    expect_equal(m[["exp_ref"]], m[["i.exp_ref"]])
    expect_equal(m[["ll"]], m[["i.ll"]])
    expect_equal(a[["word"]][1:14], b[["word"]][1:14])
    
  }
)


test_that(
  "Cooccurences-method for subcorpus and partition objects",
  {
    testthat::skip_on_cran()
    merkel <- partition(
      "GERMAPARLMINI",
      speaker = "Merkel",
      date = "2009-11-10",
      interjection = "speech",
      regex = TRUE
    )
    merkel_cooc <- Cooccurrences(
      merkel,
      p_attribute = c("word", "pos"),
      left = 3L, right = 3L,
      verbose = TRUE
    )
    ll(merkel_cooc)
    decode(merkel_cooc)
    
    expect_identical(
      unique(merkel_cooc@stat[a_word == "und"][["a_count"]]),
      count(merkel, "und")[["count"]]
    )
    
    #######
    
    merkel_sc <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-10") %>%
      subset(grep("Merkel", speaker)) %>%
      subset(interjection == "speech")
      
    merkel_cooc_sc <- Cooccurrences(
      merkel_sc,
      p_attribute = c("word", "pos"),
      left = 3L, right = 3L,
      verbose = TRUE
    )
    ll(merkel_cooc_sc)
    decode(merkel_cooc_sc)
    
    expect_identical(
      unique(merkel_cooc_sc@stat[a_word == "und"][["a_count"]]),
      count(merkel_sc, "und", verbose = FALSE)[["count"]]
    )
    
    expect_identical(merkel_cooc@stat, merkel_cooc_sc@stat)
  }
)