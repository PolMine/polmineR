test_that(
  "tooltip css centers tooltips independent of their width",
  {
    css <- readLines(system.file("css", "tooltips.css", package = "polmineR"))
    css <- paste(css, collapse = "\n")
    css <- gsub("/\\*.*?\\*/", "", css) # drop comments
    rule <- regmatches(
      css,
      regexpr("\\.tooltipping \\.tooltippingtext\\s*\\{[^}]*\\}", css)
    )
    expect_length(rule, 1L)

    # A fixed negative margin-left only centers tooltips with a fixed width (#247)
    expect_false(grepl("margin-left\\s*:\\s*-", rule))
    expect_true(grepl("transform\\s*:\\s*translateX\\(-50%\\)", rule))
    expect_false(grepl("[^-]width\\s*:\\s*[0-9]+px", rule))

    # The css is included in html documents with tooltips
    use(pkg = "RcppCWB", corpus = "REUTERS")
    h <- partition("REUTERS", places = "argentina", verbose = FALSE) %>%
      html() %>%
      highlight(lightgreen = "higher") %>%
      tooltips(list(lightgreen = "Further information"), verbose = FALSE)
    expect_true(grepl("translateX(-50%)", as.character(h), fixed = TRUE))
  }
)

test_that(
  "numeric values are accepted as tooltips",
  {
    use(pkg = "RcppCWB", corpus = "REUTERS")
    h <- corpus("REUTERS") %>%
      subset(id == "127") %>%
      read() %>%
      highlight(list(lightgreen = "oil"))

    # numeric value failed with error from xml2::xml_text<- (#260)
    num <- tooltips(h, list(lightgreen = 0.5), verbose = FALSE)
    chr <- tooltips(h, list(lightgreen = "0.5"), verbose = FALSE)
    expect_identical(as.character(num), as.character(chr))
    expect_true(
      grepl('<span class="tooltippingtext">0.5</span>', num, fixed = TRUE)
    )
  }
)
