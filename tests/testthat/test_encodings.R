library(polmineR)

testthat::context("encoding")

test_that(
  "as.corpusEnc",
  {
    mixed <- c("ä", "ö", "ü")
    Encoding(mixed)[2] <- "latin1"
    testthat::expect_warning(as.corpusEnc(mixed, from = "UTF-8", corpusEnc = "latin1"))
  }
)

test_that(
  "recoding call and quosure",
  {
    expect_identical(encoding(quote(speaker == "Müller")), "UTF-8")
    expect_identical(encoding(quote(speaker == "Meier")), "unknown")
    
    expect_identical(
      encoding(rlang::new_quosure(quote(speaker == "Müller"))),
      "UTF-8"
    )
    expect_identical(
      encoding(rlang::new_quosure(quote(speaker == "Meier"))),
      "unknown"
    )
    
    expr <- quote(speaker == "Müller")
    encoding(expr) <- "latin1"
    expect_identical(encoding(expr), "latin1")
    
    qu <- rlang::new_quosure(quote(speaker == "Müller"))
    encoding(qu) <- "latin1"
    expect_identical(encoding(qu), "latin1")
  }
)