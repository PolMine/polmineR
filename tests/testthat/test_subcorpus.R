library(polmineR)
use("polmineR")

testthat::context("subcorpus")

test_that(
  "generate subcorpus", {
    p <- partition("GERMAPARLMINI", speaker = "Angela Dorothea Merkel")
    gparl <- corpus("GERMAPARLMINI")
    sc1 <- subset(gparl, speaker == "Angela Dorothea Merkel")
    sc2 <- subset("GERMAPARLMINI", speaker == "Angela Dorothea Merkel")
    sc_p <- as(p, "subcorpus")
    
    expect_identical(size(sc_p), size(sc1))
    expect_identical(size(sc_p), size(sc2))
    
    expect_identical(p@cpos, sc1@cpos)
    expect_identical(p@cpos, sc2@cpos)
    
    expect_identical(sc_p, sc1)
    expect_identical(sc_p, sc2)
    
    p <- partition("GERMAPARLMINI", speaker = "Bärbel Höhn")
    sc <- subset(gparl, speaker == "Bärbel Höhn")
    expect_identical(size(p), size(sc))
    expect_identical(p@cpos, sc@cpos)
    expect_identical(as(p, "subcorpus"), sc)
    
    p <- partition("GERMAPARLMINI", party = c("SPD", "DIE_LINKE", "B90_DIE_GRUENEN"))
    sc <- subset(gparl, party %in% c("SPD", "DIE_LINKE", "B90_DIE_GRUENEN"))
    expect_identical(size(p), size(sc))
    expect_identical(p@cpos, sc@cpos)
    expect_identical(as(p, "subcorpus"), sc)
    
    p <- partition("GERMAPARLMINI", party = c("CDU_CSU", "FDP", "NA"))
    sc <- subset(gparl, !party %in% c("SPD", "DIE_LINKE", "B90_DIE_GRUENEN"))
    expect_identical(size(p), size(sc))
    expect_identical(p@cpos, sc@cpos)
    # expect_identical(as(p, "subcorpus"), sc) # order different
    
    pp <- partition(p, speaker = "Angela Dorothea Merkel")
    scsub <- subset(sc, grepl("Merkel", speaker))
    expect_identical(size(p), size(sc))
    expect_identical(p@cpos, sc@cpos)
  }
)


test_that(
  "methods for subcorpus objects", {
    
  }
)
