library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

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
    
    # This is also a test whether special characters are digested properly.
    if (encoding() != "UTF-8"){
      query <- "B\u00E4rbel H\u00F6hn"
      Encoding(query) <- "UTF-8"
      sp <- iconv(query, from = "UTF-8", to = encoding())
      p <- partition("GERMAPARLMINI", speaker = sp)
      sc <- subset(gparl, speaker = sp)
    } else {
      sc <- subset(gparl, speaker == "B\u00E4rbel H\u00F6hn")
      p <- partition("GERMAPARLMINI", speaker = "B\u00E4rbel H\u00F6hn")
    }
    
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
    
    # check for nested data, same result irrespective from order
    a <- corpus("GERMAPARLMINI") %>%
      subset(protocol_date == "2009-11-11") %>%
      subset(party == "SPD")
    
    b <- corpus("GERMAPARLMINI") %>%
      subset(party == "SPD") %>%
      subset(protocol_date == "2009-11-11")
    
    expect_identical(a@cpos, b@cpos)
    
    speakers <- corpus("GERMAPARLMINI") %>%
      subset(speaker = c("Merkel", "Kauder"), regex = TRUE) %>%
      s_attributes("speaker")
    expect_identical(speakers, c("Angela Dorothea Merkel", "Volker Kauder"))
  }
)

test_that(
  "programming against subset()",
  {
    # corpus as point of departure
    gparl <- corpus("GERMAPARLMINI")
    persons <- c("Angela Dorothea Merkel", "Volker Kauder", "Ronald Pofalla")
    subcorpora <- lapply(persons, function(who) subset(gparl, speaker == !!who))
    names(subcorpora) <- persons
    expect_identical(
      unname(sapply(subcorpora, s_attributes, "speaker")),
      persons
    )
    for (person in persons){
      p <- partition("GERMAPARLMINI", speaker = person)
      expect_identical(size(p), size(subcorpora[[person]]))
    }
    
    g <- corpus("GERMAPARLMINI") %>% subset(quote(speaker == "Volker Kauder"))
    expect_identical(size(g), size(subcorpora[["Volker Kauder"]]))
    
    # subcorpus as point of departure
    gparl_sub <- corpus("GERMAPARLMINI") %>% subset(date == "2009-11-10")
    persons <- c("Angela Dorothea Merkel", "Volker Kauder", "Ronald Pofalla")
    subcorpora <- lapply(persons, function(who) subset(gparl_sub, speaker == !!who))
    names(subcorpora) <- persons
    for (person in persons){
      p <- partition(as(gparl_sub, "partition"), speaker = person, verbose = FALSE)
      expect_identical(size(p), size(subcorpora[[person]]))
    }
    
    g <- corpus("GERMAPARLMINI") %>%
      subset(quote(speaker == "Volker Kauder" & date == "2009-11-10"))
    expect_identical(size(g), size(subcorpora[["Volker Kauder"]]))
  }
)


test_that(
  "warnings if s-attributes are not valid",
  {
    expect_warning({a <- corpus("REUTERS") %>% subset(foo == "127")})
    expect_null(a)
    
    expect_warning({b <- corpus("REUTERS") %>% subset(lm == "127")})
    expect_null(b)
    
    expect_warning({
      c <- corpus("REUTERS") %>% subset(id == "127" & foo == "abc")
    })
    expect_null(c)
    
    expect_warning({
      d <- corpus("REUTERS") %>% subset(lm == "127" & foo == "abc")
    })
    expect_null(d)
  }
)
