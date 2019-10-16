library(polmineR)

testthat::context("dispersion")

test_that(
  "dispersion",
  {
    y <- dispersion("GERMAPARLMINI", query = "Integration", s_attribute = "date")
    expect_identical(y[["count"]], c(1L, 0L, 7L, 15L, 0L))

    p <- partition("GERMAPARLMINI", date = "2009-11-11", p_attribute = NULL, regex = TRUE)
    int <- dispersion(p, query = "Integration", p_attribute = "word", s_attribute = "speaker")
    expect_identical(int[speaker == "Hartfrid Wolff"][["count"]], 3L)
    expect_identical(int[speaker == "Hermann Otto Solms"][["count"]], 4L)
    
    int <- dispersion("GERMAPARLMINI", "Integration", s_attribute = c("date", "party"))
    expect_equal(rowSums(int[, 2:ncol(int)]), y[["count"]])
    
    integration <- dispersion("GERMAPARLMINI", '"Integration.*"', s_attribute = "date", cqp = TRUE)
    expect_equal(integration[["count"]], c(1L, 0L, 11L, 31L, 0L))
  }
)

test_that(
  "dispersion using multiple queries",
  {
    corpus_id <- "GERMAPARLMINI"
    corpus_obj <- corpus("GERMAPARLMINI")
    queries <- c('"Arbeit.*"', '"Sozial.*"')
    s_attr <- "date"
    
    # two queries, one s-attribute, no frequencies
    y <- dispersion(corpus_obj, query = queries, cqp = TRUE, s_attribute = s_attr, freq = FALSE)
    for (i in 1L:nrow(y)){
      corpus_obj_sub <- subset(corpus_obj, date = y[["date"]][i])
      expect_equal(
        y[["count"]][i],
        sum(sapply(queries, function(q) polmineR::count(corpus_obj_sub, query = q, verbose = FALSE)[["count"]]))
      )
    }
    
    # two queries, one s-attribute, frequencies
    y <- dispersion(corpus_obj, query = queries, cqp = TRUE, s_attribute = "date", freq = TRUE)
    for (i in 1L:nrow(y)){
      corpus_obj_sub <- subset(corpus_obj, date = y[["date"]][i])
      s <- size(corpus_obj_sub)
      expect_equal(
        y[["freq"]][i],
        sum(sapply(queries, function(q) polmineR::count(corpus_obj_sub, query = q, verbose = FALSE)[["freq"]]))
      )
    }
    
    # two queries, two s-attributes, frequencies
    y <- dispersion(corpus_obj, query = queries, cqp = TRUE, s_attribute = c("date", "party"), freq = TRUE)
    for (i in 1L:nrow(y)){
      corpus_obj_sub <- subset(corpus_obj, date = y[["date"]][i]) %>% subset(party == "CDU_CSU")
      s <- size(corpus_obj_sub)
      expect_equal(
        y[["CDU_CSU"]][i],
        sum(sapply(queries, function(q) polmineR::count(corpus_obj_sub, query = q, verbose = FALSE)[["freq"]]))
      )
    }
    
  }
)

