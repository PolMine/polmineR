library(polmineR)
use("polmineR")

test_that(
  "s_attributes for corpus, without specification of s_attribute",
  {
    s_attrs <- s_attributes("GERMAPARLMINI")
    expect_equal(length(s_attrs), 7L)
    expect_equal(is.character(s_attrs), TRUE)
    expect_equal(all(s_attrs %in% c("protocol_lp", "protocol_date", "interjection", "date", "party", "speaker", "role")), TRUE)
    
    s_attrs <- corpus("GERMAPARLMINI") %>%
      s_attributes()
    expect_equal(length(s_attrs), 7L)
    expect_equal(is.character(s_attrs), TRUE)
    expect_equal(all(s_attrs %in% c("protocol_lp", "protocol_date", "interjection", "date", "party", "speaker", "role")), TRUE)
  }
)

test_that(
  "multiple s-attributes",{
    s_attr_unique <- s_attributes("GERMAPARLMINI", s_attribute = c("date", "speaker"))
    s_attr_all <- s_attributes("GERMAPARLMINI", s_attribute = c("date", "speaker"), unique = FALSE)
    
    expect_equal(class(s_attr_unique)[1], "data.table")
    expect_equal(class(s_attr_all)[1], "data.table")
    
    expect_identical(ncol(s_attr_unique), 2L)
    expect_identical(ncol(s_attr_all), 2L)
    
    expect_identical(nrow(s_attr_unique), 155L)
    expect_identical(nrow(s_attr_all), 4810L)
    
    s_attr_sub_unique <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-11") %>%
      s_attributes(s_attribute = c("date", "speaker"))
    s_attr_sub_all <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-11") %>%
      s_attributes(s_attribute = c("date", "speaker"), unique = FALSE)
    
    expect_equal(class(s_attr_sub_unique)[1], "data.table")
    expect_equal(class(s_attr_sub_all)[1], "data.table")
    
    expect_identical(ncol(s_attr_sub_unique), 2L)
    expect_identical(ncol(s_attr_sub_all), 2L)
    
    expect_identical(
      all(s_attr_unique[date == "2009-11-11"][["speaker"]] %in% s_attr_sub_unique[["speaker"]]),
      TRUE
    )
    expect_identical(
      all(s_attr_all[date == "2009-11-11"][["speaker"]] %in% s_attr_sub_all[["speaker"]]),
      TRUE
    )
    
    # If a subcorpus has only one region, the resulting data.table may be mixed up.
    x <- corpus("REUTERS") %>% subset(id == "237")
    s_attr_dt <- s_attributes(x, s_attribute = c("id", "places", "language"), unique = FALSE)
    expect_equal(nrow(s_attr_dt), nrow(x@cpos))
  }
)


test_that(
  "get s-attributes in a call or a quosure",
  {
    expect_equal(
      s_attributes(quote(grep("Merkel", speaker)), corpus = "GERMAPARLMINI"),
      setNames("speaker", "character")
    )
    expect_equal(
      s_attributes(quote(speaker == "Angela Merkel"), corpus = "GERMAPARLMINI"),
      setNames("speaker", "character")
    )
    expect_equal(
      s_attributes(
        quote(speaker == "Angela Merkel" & date == "2009-10-28"),
        corpus = "GERMAPARLMINI"
      ),
      c(character = "speaker", character = "date")
    )
    
    expect_equal(
      s_attributes(
        rlang::new_quosure(quote(grep("Merkel", speaker))),
        corpus = "GERMAPARLMINI"
      ),
      setNames("speaker", "character")
    )
    
  }
)

test_that(
  "s_attributes for subcorpus_bundle",
  {
    speeches <- corpus("GERMAPARLMINI") |>
      as.speeches(
        s_attribute_date = "protocol_date",
        s_attribute_name = "speaker",
        gap = 300,
        progress = FALSE
      )
    
    # s-attribute is sibling
    s_attrs_sibling <- s_attributes(speeches, s_attribute = "party")
    for (i in 1:10){
      expect_identical(
        s_attrs_sibling[[i]],
        s_attributes(speeches[[i]], "party")
      )
    }
    
    # s-attribute is ancestor
    s_attrs_ancestor <- s_attributes(speeches, s_attribute = "protocol_date")
    for (i in 1:10){
      expect_identical(
        s_attrs_ancestor[[i]],
        s_attributes(speeches[[i]], "protocol_date")
      )
    }

    daywise <- corpus("GERMAPARLMINI") %>%
      split(s_attribute = "protocol_date", verbose = FALSE)
    
    s_attrs <- s_attributes(daywise, "speaker")
    
    for (i in seq_along(daywise)){
      expect_identical(s_attrs[[i]], s_attributes(daywise[[i]], "speaker"))
    }
    
  }
)
test_that(
  "s_attributes() for s-attribute not present in (sub)corpus (#287)",
  {
    tmp <- tempfile()
    vrt_dir <- file.path(tmp, "vrt")
    data_dir <- file.path(tmp, "data")
    registry <- file.path(tmp, "registry")
    for (d in c(vrt_dir, data_dir, registry)) dir.create(d, recursive = TRUE)
    writeLines(
      c(
        '<doc id="d1">',
        '<p type="speech">',
        '<ne type="PER">', "Angela\tNE\tAngela", "Merkel\tNE\tMerkel", "</ne>",
        "spricht\tVVFIN\tsprechen", "in\tAPPR\tin",
        '<ne type="LOC">', "Berlin\tNE\tBerlin", "</ne>",
        "</p>",
        '<p type="interjection">', "Beifall\tNN\tBeifall", ".\t$.\t.", "</p>",
        '<p type="speech">', "Danke\tNN\tDanke", ".\t$.\t.", "</p>",
        "</doc>"
      ),
      file.path(vrt_dir, "netest.vrt")
    )
    RcppCWB::cwb_encode(
      corpus = "NETEST", registry = registry, data_dir = data_dir,
      vrt_dir = vrt_dir, p_attributes = c("word", "pos", "lemma"),
      s_attributes = list(doc = "id", p = "type", ne = "type"), quietly = TRUE
    )
    RcppCWB::cqp_load_corpus("NETEST", registry = registry)

    paragraphs <- corpus("NETEST", registry_dir = registry) %>%
      split(s_attribute = "p_type", values = FALSE, verbose = FALSE, progress = FALSE)

    # subcorpus
    expect_identical(s_attributes(paragraphs[[1]], "ne_type"), c("PER", "LOC"))
    expect_no_warning(y <- s_attributes(paragraphs[[2]], "ne_type"))
    expect_identical(y, character())

    # subcorpus_bundle
    expect_no_warning(y <- s_attributes(paragraphs, "ne_type"))
    expect_length(y, 3L)
    expect_identical(y[[1]], c("PER", "LOC"))
    expect_identical(y[[2]], character())
    expect_identical(y[[3]], character())
  }
)

test_that(
  "s_attributes() for corpus with nested s-attributes (#245)",
  {
    s_attrs <- c("speaker", "party", "protocol_lp")

    y <- s_attributes("GERMAPARLMINI", s_attribute = s_attrs)
    expect_identical(colnames(y), s_attrs)

    # same result as combining the results for daywise subcorpora (regions
    # of subcorpora created by subset() are the regions of s-attribute 'date')
    y_days <- lapply(
      s_attributes("GERMAPARLMINI", "date"),
      function(day){
        corpus("GERMAPARLMINI") %>%
          subset(date == !!day) %>%
          s_attributes(s_attribute = s_attrs)
      }
    ) %>%
      data.table::rbindlist() %>%
      unique()
    expect_true(data.table::fsetequal(y, y_days))

    # column names also for subcorpus with only one region
    sc <- corpus("GERMAPARLMINI") %>% subset(protocol_date == "2009-10-27")
    expect_identical(nrow(slot(sc, "cpos")), 1L)
    expect_identical(colnames(s_attributes(sc, s_attrs)), s_attrs)

    # one row per region of the first s-attribute if unique is FALSE
    y_all <- s_attributes("GERMAPARLMINI", s_attribute = s_attrs, unique = FALSE)
    expect_identical(colnames(y_all), s_attrs)
    expect_identical(
      nrow(y_all),
      RcppCWB::cl_attribute_size(
        "GERMAPARLMINI", attribute = "speaker", attribute_type = "s",
        registry = corpus_registry_dir("GERMAPARLMINI")
      )
    )
  }
)
