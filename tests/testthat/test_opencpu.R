use("polmineR")

# Tests require an OpenCPU server with polmineR installed. The URL of the
# server is taken from the environment variable 'OPENCPU_SERVER', for instance
# "http://localhost:8004" when running the Docker image
# ghcr.io/polmine/polminer-opencpu-dev locally (port 8004), or
# "https://cloud.opencpu.org". Tests are skipped if it is not set.

ocpu_server <- Sys.getenv("OPENCPU_SERVER")

skip_if_no_ocpu <- function(){
  skip_on_cran()
  skip_if(!nzchar(ocpu_server), "environment variable OPENCPU_SERVER not set")
  skip_if_not_installed("httr2")
  skip_if_not_installed("protolite")
  skip_if_not_installed("curl")
}

test_that(
  "opencpu server - subsetting",
  {
    skip_if_no_ocpu()

    gparl <- corpus("GERMAPARLMINI", server = ocpu_server, restricted = FALSE)

    gparl_sub1 <- subset(gparl, date == "2009-11-11")
    gparl_sub2 <- subset(gparl, substitute(date == "2009-11-11"))

    expect_identical(gparl_sub1, gparl_sub2)
  }
)

test_that(
  "opencpu server - subsetting with substitute() and variables",
  {
    skip_if_no_ocpu()
    skip("remote subset() returns NULL for substitute() with variables (#299)")

    gparl <- corpus("GERMAPARLMINI", server = ocpu_server, restricted = FALSE)
    gparl_sub1 <- subset(gparl, date == "2009-11-11")
    day <- "2009-11-11"
    gparl_sub3 <- subset(gparl, substitute(date == day, list(day = day)))
    expect_identical(gparl_sub1, gparl_sub3)

    days <- c("FAS_200109_11098", "2009-11-12")
    gparl_sub4 <- subset(gparl, substitute(date %in% days, list(days = days)))

    gparl_sub_local <- corpus("GERMAPARLMINI") %>%
      subset(substitute(date %in% days, list(days = days)))

    expect_identical(size(gparl_sub4), size(gparl_sub_local))
  }
)

test_that(
  "opencpu server - s_attributes",
  {
    skip_if_no_ocpu()

    gparl <- corpus("GERMAPARLMINI", server = ocpu_server, restricted = FALSE)
    gparl_s_attr <- s_attributes(gparl)
    gparl_s_attr_local <- s_attributes("GERMAPARLMINI")
    expect_identical(gparl_s_attr, gparl_s_attr_local)
  }
)

test_that(
  "opencpu server - error message from server is passed on (#296)",
  {
    skip_if_no_ocpu()

    expect_error(
      ocpu_exec(fn = "corpus", server = ocpu_server, .Object = "FOOBAR"),
      "FOOBAR"
    )
  }
)

test_that(
  "ocpu_exec() checks that corpus property 'user' is defined",
  {
    skip_if_not_installed("httr2")
    skip_if_not_installed("protolite")
    skip_if_not_installed("curl")

    registry <- tempfile()
    dir.create(registry)
    regfile <- system.file(
      package = "RcppCWB", "extdata", "cwb", "registry", "reuters"
    )
    # A corpus id of its own avoids that REUTERS is loaded from two registries.
    # Corpus properties have to precede attribute declarations.
    reglines <- sub("^ID\\s+reuters$", "ID   ocputest", readLines(regfile))
    writeLines(
      append(
        reglines, '##:: password = "secret"',
        after = grep("^##:: language", reglines)
      ),
      file.path(registry, "ocputest")
    )
    old <- Sys.getenv("OPENCPU_REGISTRY")
    Sys.setenv(OPENCPU_REGISTRY = registry)
    on.exit(Sys.setenv(OPENCPU_REGISTRY = old))

    expect_error(
      ocpu_exec(
        fn = "size", corpus = "OCPUTEST", server = "http://localhost:1",
        restricted = TRUE, .Object = "OCPUTEST"
      ),
      "property 'user' required but not found"
    )
  }
)
