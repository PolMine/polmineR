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
