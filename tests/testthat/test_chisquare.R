library(polmineR)
testthat::context("chisquare")
use("polmineR")

test_that(
  "check chisquare computation",
  {
    m <- partition(
      "GERMAPARLMINI", speaker = "Merkel", interjection = "speech",
      regex = TRUE, p_attribute = "word"
    )
    f <- features(m, "GERMAPARLMINI", included = TRUE)
    f_min <- subset(f, count_coi >= 5)
    f_min_sorted <- sort(f_min, by = "chisquare")

    for (token in f_min_sorted@stat[["word"]][1:10]){
      o <- matrix(data = rep(NA, 4), ncol = 2)
      o[1,1] <- m@stat[word == token][["count"]]
      o[1,2] <- count("GERMAPARLMINI", query = token)[["count"]] - o[1,1]
      o[2,1] <- size(f)[["coi"]] - o[1,1]
      o[2,2] <- size(f)[["ref"]] - o[1,2]

      r <- rowSums(o)
      c <- colSums(o)
      N <- sum(o)
      
      e <- matrix(data = rep(NA, 4), ncol = 2)
      e[1,1] <- r[1] * (c[1] / N)
      e[1,2] <- r[1] * (c[2] / N)
      e[2,1] <- r[2] * (c[1] / N)
      e[2,2] <- r[2] * (c[2] / N)
      
      y <- matrix(rep(NA, 4), ncol = 2)
      for (i in 1:2) for (j in 1:2) y[i,j] <- (o[i,j] - e[i,j])^2 / e[i,j]
      chisquare_value_selfmade <- sum(y)
      
      chisquare_value_fn <- as(f, "data.table")[word == token][["chisquare"]]
      expect_equal(chisquare_value_selfmade, chisquare_value_fn)
    }
  }
)
  