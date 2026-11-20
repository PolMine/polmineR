library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

test_that(
  "message on registry used if corpus is loaded multiple times",
  {
    library(RcppCWB)
    expect_identical(
      length(RcppCWB::corpus_registry_dir("REUTERS")),
      2L
    )
    expect_message(corpus("REUTERS"))
  }
)

test_that(
  "show-method for corpus reports corpus ID, name and version",
  {
    y <- capture_messages(show(corpus("GERMAPARLMINI")))
    expect_true(any(grepl("^corpus ID:\\s+GERMAPARLMINI", y)))
    expect_true(any(grepl(sprintf("^name:\\s+%s", corpus("GERMAPARLMINI")@name), y)))
    expect_true(any(grepl("^version:\\s+0.1.0", y)))

    # REUTERS does not state a version in its registry file
    y <- capture_messages(show(corpus("REUTERS")))
    expect_true(any(grepl("^corpus ID:\\s+REUTERS", y)))
    expect_true(any(grepl("^version:\\s+not available", y)))

    # A subcorpus reports the full name of the corpus
    sc <- subset(corpus("GERMAPARLMINI"), speaker == "Angela Dorothea Merkel")
    y <- capture_messages(show(sc))
    expect_true(any(grepl(sprintf("^name:\\s+%s", corpus("GERMAPARLMINI")@name), y)))
  }
)
