library(polmineR)
testthat::context("split")
use("polmineR")


test_that(
  "split up corpus",
  {
    x <- partition("GERMAPARLMINI", date = "2009-11-11", speaker = "Gerda Hasselfeldt")
    y <- split(x, gap = 500)
    expect_identical(is(y)[1], "partition_bundle")
    expect_identical(length(y), 15L)
    expect_identical(size(merge(y)), size(x))
    
    y2 <- merge(y)
    expect_identical(as.integer(merge(y)@cpos), as.integer(x@cpos))
    expect_identical(as.vector(merge(y)@cpos), as.vector(x@cpos))
    
    # check that argument values works as intended
    speakers <- c("Volker Kauder", "Norbert Lammert", "Wolfgang Thierse")
    sb_speakers <- corpus("GERMAPARLMINI") %>% 
      split(s_attribute = "speaker", values = speakers)
    expect_true(all(speakers %in% names(sb_speakers)))
    
    
    # the following tests require that GERMAPARL2MINI is available
    # It is wrapped into the GermaParl2 package, which can be installed as 
    # follows:
    # install.packages(
    #   pkgs = "GermaParl2",
    #   contriburl = "https://polmine.github.io/drat/src/contrib",
    #   type = "source"
    # )
    
    skip_if_not(use("GermaParl2"))
    
    gparl2 <- corpus("GERMAPARL2MINI")
    
    n_sentences <- gparl2 %>% 
      split(s_attribute = "p", values = FALSE, verbose = FALSE) %>% 
      length()
    
    attr_size <- RcppCWB::cl_attribute_size(
      corpus = "GERMAPARL2MINI",
      attribute = "p",
      attribute_type = "s",
      registry = gparl2@registry_dir
    )
    
    expect_identical(n_sentences, attr_size)
    
  }
)

test_that(
  "different order, same result",
  {
    pp1 <- corpus("GERMAPARLMINI") %>%
      subset(protocol_date == "2009-11-10") %>%
      split(s_attribute = "speaker")
    
    pp2 <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-10") %>%
      split(s_attribute = "speaker")
    
    pp3 <- corpus("GERMAPARLMINI") %>%
      split(s_attribute = "speaker") %>%
      .[["Angela Dorothea Merkel"]] %>%
      subset(date == "2009-11-10")
    
    dimnames(pp3@cpos) <- NULL
    
    expect_identical(
      pp1[["Angela Dorothea Merkel"]]@cpos,
      pp2[["Angela Dorothea Merkel"]]@cpos
    )
    
    expect_identical(
      pp1[["Angela Dorothea Merkel"]]@cpos,
      pp3@cpos
    )
    
    skip_if_not(use("GermaParl2"))
    
    gparl2 <- corpus("GERMAPARL2MINI")
    
    renner <- gparl2 %>% 
      subset(speaker_who == "Renner")

    n_subcorpora <- split(renner, s_attribute = "s", verbose = FALSE) |>
      length()
    
    n_sentences <- renner |>
      slot("cpos") %>% 
      RcppCWB::ranges_to_cpos() %>% 
      RcppCWB::cl_cpos2struc(corpus = "GERMAPARL2MINI", s_attribute = "s", cpos = ., registry = gparl2@registry_dir) %>% 
      unique() %>% 
      length()
    
    expect_identical(n_subcorpora, n_sentences)
  }
)

