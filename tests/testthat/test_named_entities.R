library(polmineR)
use("polmineR")

test_that(
  "ne_attributes() returns NULL if named entities are not declared",
  {
    expect_null(ne_attributes("GERMAPARLMINI"))
    expect_null(ne_attributes(corpus("GERMAPARLMINI")))
  }
)

test_that(
  "ne_attributes() reads a full declaration",
  {
    registry <- ne_test_corpus(
      id = "netestfull",
      properties = c(
        '##:: ne_s_attribute = "speaker"',
        '##:: ne_type_s_attribute = "party"',
        '##:: ne_uri_s_attribute = "role"',
        '##:: ne_types = "PERSON | ORGANIZATION|LOCATION|MISC"'
      )
    )
    y <- ne_attributes(corpus("NETESTFULL", registry_dir = registry))
    expect_identical(names(y), c("s_attribute", "type", "uri", "types"))
    expect_identical(y[["s_attribute"]], "speaker")
    expect_identical(y[["type"]], "party")
    expect_identical(y[["uri"]], "role")
    expect_identical(
      y[["types"]], c("PERSON", "ORGANIZATION", "LOCATION", "MISC")
    )

    # character method and subcorpus give the same result as the corpus
    expect_identical(ne_attributes("NETESTFULL"), y)
    sc <- corpus("NETESTFULL", registry_dir = registry) %>%
      subset(party == "SPD")
    expect_identical(ne_attributes(sc), y)
  }
)

test_that(
  "ne_attributes() fills in optional properties",
  {
    registry <- ne_test_corpus(
      id = "netestminimal",
      properties = c(
        '##:: ne_s_attribute = "speaker"',
        '##:: ne_type_s_attribute = "party"'
      )
    )
    x <- corpus("NETESTMINIMAL", registry_dir = registry)
    y <- ne_attributes(x)
    expect_identical(y[["uri"]], NA_character_)
    # without property 'ne_types', types are the values of the type s-attribute
    expect_identical(y[["types"]], sort(s_attributes(x, "party")))

    # a subcorpus with only some types still gets all types of the corpus
    sc <- subset(x, party == "SPD")
    expect_identical(ne_attributes(sc)[["types"]], y[["types"]])
  }
)

test_that(
  "ne_attributes() checks that declared s-attributes are present",
  {
    registry <- ne_test_corpus(
      id = "netestinvalid",
      properties = c(
        '##:: ne_s_attribute = "ne"',
        '##:: ne_type_s_attribute = "ne_type"'
      )
    )
    expect_error(
      ne_attributes(corpus("NETESTINVALID", registry_dir = registry)),
      "not available in corpus 'NETESTINVALID': ne, ne_type"
    )
  }
)

test_that(
  "ne_attributes() requires a length-one character vector",
  {
    expect_error(
      ne_attributes(c("GERMAPARLMINI", "REUTERS")),
      "length-one character vector"
    )
  }
)

# s-attributes of GERMAPARLMINI stand in for named-entity annotation: speaker
# regions are mentions, party is the type, and interjection the URI. Regions
# of 'interjection' have the same boundaries as speaker regions, so all
# mentions are "linked".
ne_registry <- ne_test_corpus(
  id = "netestclass",
  properties = c(
    '##:: ne_s_attribute = "speaker"',
    '##:: ne_type_s_attribute = "party"',
    '##:: ne_uri_s_attribute = "interjection"'
  )
)

ne_regions <- function(s_attribute){
  n <- RcppCWB::cl_attribute_size(
    "NETESTCLASS", attribute = s_attribute, attribute_type = "s",
    registry = ne_registry
  )
  m <- RcppCWB::get_region_matrix(
    "NETESTCLASS", s_attribute = s_attribute, strucs = 0L:(n - 1L),
    registry = ne_registry
  )
  data.frame(
    struc = 0L:(n - 1L), cpos_left = m[, 1], cpos_right = m[, 2],
    value = RcppCWB::cl_struc2str(
      "NETESTCLASS", s_attribute = s_attribute, struc = 0L:(n - 1L),
      registry = ne_registry
    )
  )
}

test_that(
  "named_entities() gets mentions in a corpus",
  {
    speaker <- ne_regions("speaker")
    ne <- named_entities("NETESTCLASS")
    expect_s4_class(ne, "named_entities")
    expect_true(is(ne, "subcorpus"))
    expect_identical(slot(ne, "strucs"), speaker[["struc"]])
    expect_identical(unname(slot(ne, "cpos")[, 1]), speaker[["cpos_left"]])
    expect_identical(slot(ne, "s_attribute_strucs"), "speaker")
    expect_identical(
      size(ne),
      as.integer(sum(speaker[["cpos_right"]] - speaker[["cpos_left"]] + 1L))
    )
    expect_identical(ne, named_entities(corpus("NETESTCLASS")))
  }
)

test_that(
  "named_entities() selects mentions by type and within regions",
  {
    party <- ne_regions("party")
    cdu <- named_entities("NETESTCLASS", type = "CDU_CSU")
    expect_identical(slot(cdu, "strucs"), party[["struc"]][party[["value"]] == "CDU_CSU"])

    # mentions within the regions of a subcorpus
    sc <- subset(corpus("NETESTCLASS"), date == "2009-11-10")
    ne <- named_entities(sc)
    speaker <- ne_regions("speaker")
    within <- vapply(
      seq_len(nrow(speaker)),
      function(i) any(
        speaker[["cpos_left"]][i] >= slot(sc, "cpos")[, 1] &
          speaker[["cpos_right"]][i] <= slot(sc, "cpos")[, 2]
      ),
      FUN.VALUE = logical(1L)
    )
    expect_identical(slot(ne, "strucs"), speaker[["struc"]][within])

    # a named_entities object is a subcorpus, so the constructor can select
    # mentions by type
    spd <- named_entities(ne, type = "SPD")
    expect_s4_class(spd, "named_entities")
    expect_identical(
      slot(spd, "strucs"),
      intersect(slot(ne, "strucs"), party[["struc"]][party[["value"]] == "SPD"])
    )

    expect_error(
      named_entities("NETESTCLASS", type = "XYZ"),
      "type\\(s\\) not available: XYZ"
    )
    expect_error(
      named_entities("GERMAPARLMINI"),
      "does not declare named-entity annotation"
    )
  }
)

test_that(
  "count() counts mentions of named entities",
  {
    ne <- named_entities("NETESTCLASS", type = "CDU_CSU")
    cnt <- count(ne)
    expect_s4_class(cnt, "count")
    expect_identical(slot(cnt, "p_attribute"), "surface")
    expect_identical(slot(cnt, "size"), nrow(slot(ne, "cpos")))
    expect_identical(colnames(slot(cnt, "stat")), c("surface", "type", "count"))
    expect_identical(sum(slot(cnt, "stat")[["count"]]), nrow(slot(ne, "cpos")))
    expect_true(all(slot(cnt, "stat")[["type"]] == "CDU_CSU"))
    expect_false(is.unsorted(rev(slot(cnt, "stat")[["count"]])))

    # the surface form of a mention are its tokens, separated by a space
    first <- slot(ne, "cpos")[1, ]
    surface <- paste(
      RcppCWB::cl_cpos2str(
        "NETESTCLASS", p_attribute = "word", cpos = first[1]:first[2],
        registry = ne_registry
      ),
      collapse = " "
    )
    surface <- as.nativeEnc(surface, from = "latin1")
    expect_true(surface %in% slot(cnt, "stat")[["surface"]])

    # by URI: mentions are linked if a URI region has the same boundaries
    ne <- named_entities("NETESTCLASS")
    cnt <- count(ne, by = "uri")
    expect_identical(slot(cnt, "p_attribute"), "uri")
    expect_identical(
      colnames(slot(cnt, "stat")),
      c("uri", "type", "count", "surface", "n_surface")
    )
    stat <- slot(cnt, "stat")
    expected <- table(ne_regions("interjection")[["value"]])
    expect_identical(
      stat[["count"]][match(names(expected), stat[["uri"]])],
      as.integer(expected)
    )
    expect_false(anyNA(stat[["uri"]]))

    # Regions of 'protocol_date' include all mentions, but do not have the
    # same boundaries, so no mention is linked.
    registry <- ne_test_corpus(
      id = "netestunlinked",
      properties = c(
        '##:: ne_s_attribute = "speaker"',
        '##:: ne_uri_s_attribute = "protocol_date"'
      )
    )
    stat <- slot(count(named_entities("NETESTUNLINKED"), by = "uri"), "stat")
    expect_identical(nrow(stat), 1L)
    expect_true(is.na(stat[["uri"]]))
    expect_identical(stat[["count"]], nrow(ne_regions("speaker")))

    # count objects of mentions can be compared with features()
    spd <- count(named_entities("NETESTCLASS", type = "SPD"))
    cdu <- count(named_entities("NETESTCLASS", type = "CDU_CSU"))
    expect_s4_class(features(spd, cdu, included = FALSE, verbose = FALSE), "features")
  }
)

test_that(
  "named_entities() and count() check the declaration",
  {
    registry <- ne_test_corpus(
      id = "netestnotype",
      properties = '##:: ne_s_attribute = "speaker"'
    )
    expect_error(
      named_entities("NETESTNOTYPE", type = "SPD"),
      "requires that the corpus declares a type s-attribute"
    )
    ne <- named_entities("NETESTNOTYPE")
    expect_true(all(is.na(slot(count(ne), "stat")[["type"]])))
    expect_error(
      count(ne, by = "uri"),
      "requires that the corpus declares a URI s-attribute"
    )
  }
)

test_that(
  "empty values of named-entity s-attributes are not available",
  {
    registry <- ne_vrt_corpus()
    expect_identical(
      RcppCWB::cl_struc2str(
        "NETESTVRT", s_attribute = "ne_uri", struc = 0:4, registry = registry
      ),
      c("Q567", "Q64", "Q64", "", "")
    )

    # without property 'ne_types', an empty type is not a type
    expect_identical(ne_attributes("NETESTVRT")[["types"]], c("LOCATION", "PERSON"))

    ne <- named_entities("NETESTVRT")
    stat <- slot(count(ne, by = "uri"), "stat")
    expect_identical(stat[["uri"]], c("Q64", NA, "Q567"))
    expect_identical(stat[["count"]], c(2L, 2L, 1L))

    stat <- slot(count(ne), "stat")
    expect_identical(stat[stat[["surface"]] == "Ostpolitik"][["type"]], NA_character_)

    # mentions without a type are not selected by type
    expect_identical(slot(named_entities("NETESTVRT", type = "PERSON"), "strucs"), c(0L, 3L))
  }
)

test_that(
  "count() reports further attributes of mentions",
  {
    registry <- ne_vrt_corpus(id = "netestattrs")
    ne <- named_entities("NETESTATTRS")

    stat <- slot(count(ne, by = "uri", mention_attributes = "ne_wikidata_type"), "stat")
    expect_identical(
      colnames(stat),
      c("uri", "type", "count", "surface", "n_surface", "ne_wikidata_type")
    )
    expect_identical(stat[["uri"]], c("Q64", NA, "Q567"))
    # empty values (of unlinked mentions) are NA
    expect_identical(
      stat[["ne_wikidata_type"]], c("|Q515|Q1549591|", NA, "|Q5|")
    )

    # by surface form, the most frequent value is reported
    stat <- slot(count(ne, mention_attributes = "ne_wikidata_type"), "stat")
    expect_identical(
      stat[["ne_wikidata_type"]][match("Berlin", stat[["surface"]])],
      "|Q515|Q1549591|"
    )

    # without mention_attributes, the result is the same as before
    expect_identical(
      colnames(slot(count(ne, by = "uri"), "stat")),
      c("uri", "type", "count", "surface", "n_surface")
    )

    expect_error(
      count(ne, by = "uri", mention_attributes = "ne_xyz"),
      "s-attribute\\(s\\) not available: ne_xyz"
    )
    # 'doc' is an s-attribute, but not an attribute of mentions
    expect_error(
      count(ne, by = "uri", mention_attributes = "doc_id"),
      "without one region for each mention: doc_id"
    )
  }
)
