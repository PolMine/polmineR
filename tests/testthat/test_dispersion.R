library(polmineR)
use("polmineR")

testthat::context("dispersion")

test_that(
  "dispersion",
  {
    y1 <- dispersion("GERMAPARLMINI", query = "Integration", s_attribute = "date", fill = TRUE)
    expect_identical(y1[["count"]], c(1L, 0L, 7L, 15L, 0L))
    
    y2 <- dispersion("GERMAPARLMINI", '"Integration.*"', s_attribute = "date", cqp = TRUE, fill = TRUE)
    expect_equal(y2[["count"]], c(1L, 0L, 11L, 31L, 0L))
    
    y3 <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-11") %>%
      dispersion(query = "Integration", p_attribute = "word", s_attribute = "speaker", fill = FALSE)
    expect_identical(y3[speaker == "Hartfrid Wolff"][["count"]], 3L)
    expect_identical(y3[speaker == "Hermann Otto Solms"][["count"]], 4L)
    
    y4 <- dispersion("GERMAPARLMINI", "Integration", s_attribute = c("date", "party"), fill = TRUE)
    expect_equal(rowSums(y4[, 2:ncol(y4)]), y1[["count"]])
    s_attr_party_values <- s_attributes("GERMAPARLMINI", "party")
    expect_true(all(colnames(y4)[2:ncol(y4)] %in% s_attr_party_values))
    expect_identical(length(s_attr_party_values), ncol(y4) - 1L)
    s_attr_date_values <- s_attributes("GERMAPARLMINI", "date")
    expect_true(all(y4[[1]] %in% s_attr_date_values))
    expect_identical(length(s_attr_date_values), nrow(y4))
    
    y5 <- dispersion("GERMAPARLMINI", "Integration", s_attribute = c("date", "party"), fill = FALSE)
    expect_identical(sum(y5[, 2:ncol(y5)]), sum(y4[, 2:ncol(y4)]))
    
    y6 <- corpus("GERMAPARLMINI") %>%
      subset(speaker == "Angela Dorothea Merkel") %>%
      dispersion(query = "Integration", s_attribute = "date", fill = TRUE)
    expect_identical(y6[["count"]], c(0L, 3L))
  }
)

test_that(
  "check that warnings are issued if argument sAttribute is used",
  {
    expect_snapshot({
      expect_warning(
        y <- dispersion("GERMAPARLMINI", query = "Integration", sAttribute = "date")
      )
    })
    expect_snapshot({
      expect_warning(
        y <- corpus("GERMAPARLMINI") %>%
          subset(speaker = "Angela Dorothea Merkel") %>% 
          dispersion(query = "Integration", sAttribute = "date")
      )
    })
    expect_snapshot(
      expect_warning(
        y <- corpus("GERMAPARLMINI") %>%
          hits(query = "Integration", s_attribute = "date") %>%
          dispersion(sAttribute = "date", source = "GERMAPARLMINI")
      )
    )
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

test_that(
  "dispersion on partition",
  {
    use("polmineR")
    p <- partition("GERMAPARLMINI", date = "2009-11-10")
    y <- dispersion(p, query = "Arbeit", s_attribute = "date", freq = TRUE)
    expect_identical(y[["count"]], 37L)
  }
)
