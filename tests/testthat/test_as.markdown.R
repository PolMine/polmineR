library(polmineR)
use("polmineR")
testthat::context("as.markdown")


test_that(
  "as.markdown",
  {
    p <- partition("REUTERS", id = "127", regex = TRUE)
    y <- as.markdown(p)
    if (.Platform$OS.type == "windows") y <- iconv(y, from = localeToCharset()[1], to = "UTF-8")
    
    refdoc <- system.file(package = "polmineR", "fulltext", "reuters_as_markdown.txt")
    benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
    benchmark <- paste(benchmark, collapse = "\n")
    benchmark <- paste(benchmark, "\n", sep = "")
    
    expect_identical(nchar(y), nchar(benchmark))
    expect_identical(y, benchmark)
    
    y <- as.character(html(p))

    refdoc <- system.file(package = "polmineR", "fulltext", "reuters_as_html.html")
    benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
    benchmark <- gsub("\n\n", "\n", benchmark)
    benchmark <- paste(benchmark, collapse = "\n")
    benchmark <- paste(benchmark, "\n", sep = "")
    expect_identical(nchar(y), nchar(benchmark))
    expect_identical(y, benchmark)
  }
)


test_that(
  "plpr_partition",
  {
    m <- partition("GERMAPARLMINI", date = "2009-10-28", speaker = "Merkel", regex = TRUE) %>%
      as.speeches(s_attribute_name = "speaker", gap = 100) %>%
      .[[2]]
    y <- as.markdown(m)
    if (.Platform$OS.type == "windows") y <- iconv(y, from = localeToCharset()[1], to = "UTF-8")
    
    refdoc <- system.file(package = "polmineR", "fulltext", "merkel_as_markdown.txt")
    benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
    benchmark <- paste(benchmark, collapse = "\n")
    expect_identical(nchar(y), nchar(benchmark))
    expect_identical(y, benchmark)
    
    y <- as.character(read(m, beautify = TRUE))

    refdoc <- system.file(package = "polmineR", "fulltext", "merkel_as_html.html")
    benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
    benchmark <- gsub("\n\n", "\n", benchmark)
    benchmark <- paste(benchmark, collapse = "\n")
    benchmark <- paste(benchmark, "\n", sep = "")
    expect_identical(nchar(y), nchar(benchmark))
    expect_identical(y, benchmark)
  }
)

test_that(
  "read",
  {
    a <- subset(corpus("REUTERS"), places = "argentina")
    b <- html(a)
    c <- highlight(b, lightgreen = "higher")
    d <- tooltips(c, list(lightgreen = "Further information"))
    
    y <- read(
      a, meta = "places",
      highlight = list(lightgreen = "higher"),
      tooltips = list(lightgreen = "Further information")
    )
    
    expect_identical(d, y)
    
    refdoc <- system.file(package = "polmineR", "fulltext", "reading_reuters.html")
    benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
    benchmark <- paste(benchmark, collapse = "\n")
    benchmark <- paste(benchmark, "\n", sep = "")
    expect_identical(nchar(y), nchar(benchmark))
    expect_identical(as.character(y), benchmark)
  }
)
