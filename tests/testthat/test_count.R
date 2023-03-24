library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("count-method")

test_that("count",{
  dt <- count("REUTERS")
  expect_true(all(colnames(dt) %in% c("word", "word_id", "count")))
  expect_true(is.integer(dt[["count"]]))
  expect_true(is.integer(dt[["word_id"]]))
  expect_true(is.character(dt[["word"]]))
  expect_equal(sum(dt[["count"]]), 4050)
  expect_equal(dt@stat[word == "barrel"][["count"]], 15)
})



reuters <- partition("REUTERS", list(id = ".*"), regex = TRUE)

test_that("count (one query)", {
  expect_equal(count(reuters, query = "is")[["count"]], 25)
})

test_that("count (multiple queries)", {
  queries <- c("is", "this", "real")
  reuters_cnt <- corpus("REUTERS") %>% count(query = queries)
  expect_equal(reuters_cnt[["count"]], c(25, 7, 3))
  
  reuters_kuwait <- corpus("REUTERS") %>% subset(grepl("kuwait", places))
  reuters_kuwait_cnt <- count(reuters_kuwait, query = queries, breakdown = FALSE)
  expect_equal(reuters_kuwait_cnt[["count"]], c(3L, 3L, 0L))
  
  reuters_kuwait_partition <- partition("REUTERS", places = "kuwait", regex = TRUE)
  reuters_kuwait_cnt2 <- count(reuters_kuwait_partition, query = queries)
  expect_equal(reuters_kuwait_cnt2[["count"]], c(3L, 3L, 0L))
  
  
  # issue warning when query matches overlap
  testthat::expect_warning(
    corpus("REUTERS") %>% count(query = c('"price.*"', '"prices"'), cqp = TRUE)
  )
  testthat::expect_warning(
    count(reuters_kuwait, query = c('"price.*"', '"prices"'), cqp = TRUE)
  )
  testthat::expect_warning(
    count(reuters_kuwait_partition, query = c('"price.*"', '"prices"'), cqp = TRUE)
  )
  
  
})


test_that("count - breakdown", {
  y <- count("REUTERS", query = '"remain.*"', breakdown = TRUE)
  expect_equal(sum(y[["count"]]), 5)
})


test_that(
  "count over partition_bundle",
  {
    cnt_int_total <- corpus("GERMAPARLMINI") %>% count(query = "Integration")
    sp_bundle <- corpus("GERMAPARLMINI") %>%
      as.speeches(s_attribute_name = "speaker", s_attribute_date = "date")
    cnt_int_pb <- count(sp_bundle, query = "Integration") %>%
      subset(Integration > 0)
    expect_equal(sum(cnt_int_pb[["TOTAL"]]), cnt_int_total[["count"]])
    for (i in 1L:nrow(cnt_int_pb)){
      exp <- count(sp_bundle[[cnt_int_pb[["partition"]][i]]], query = "Integration")[["count"]]
      expect_equal(exp, cnt_int_pb[["TOTAL"]][i])
    }
  })




test_that(
  "count over partition_bundle with phrases",
  {
    obs <- corpus("GERMAPARLMINI") %>% count(p_attribute = "word")
    
    phrases <- corpus("GERMAPARLMINI") %>%
      ngrams(n = 2L, p_attribute = "word") %>%
      pmi(observed = obs) %>% 
      subset(ngram_count > 5L) %>%
      subset(1:100) %>%
      as.phrases()
    
    speeches <- corpus("GERMAPARLMINI") %>%
      as.speeches(s_attribute_name = "speaker", s_attribute_date = "date", progress = FALSE)
    
    dtm <- count(speeches, phrases = phrases, p_attribute = "word", progress = FALSE, verbose = TRUE) %>%
      as.DocumentTermMatrix(col = "count", verbose = FALSE)
    
    queries <- c(
      "erneuerbaren_Energien" = '"erneuerbaren" "Energien"',
      "Vereinten_Nationen" = '"Vereinten" "Nationen"',
      "gesetzlichen_Mindestlohn" = '"gesetzlichen" "Mindestlohn"'
    )
    matches <- count(speeches, query = queries, cqp = TRUE, progress = FALSE) %>%
      subset(TOTAL > 0)
    
    for (i in 1:nrow(matches)){
      expect_equal(matches[['TOTAL']][i], sum(as.vector(dtm[matches$partition[i], names(queries)])))
    }
    
  }
)

test_that(
  "equivalence of using arg p_attribute and CQP syntax for count,partition_bundle",
  {
    speeches <- corpus("GERMAPARLMINI") |>
      as.speeches(s_attribute_date = "date", s_attribute_name = "speaker")
    
    a <- count(speeches, query = '[pos = "ADJA"]', cqp = TRUE)
    b <- count(speeches, query = "ADJA", p_attribute = "pos")
    
    expect_identical(a$TOTAL, b$TOTAL)
    
  }
)

test_that(
  "issue warning if there are overlapping queries",
  {
  }
)