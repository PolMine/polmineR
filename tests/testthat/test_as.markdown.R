library(polmineR)
use("polmineR")
use(pkg = "RcppCWB", corpus = "REUTERS")

testthat::context("as.markdown")


test_that(
  "as.markdown",
  {
    # Running these tests on CRAN would be too limiting for development of
    # markdown pkg
    skip_on_cran()
    
#     p <- partition("REUTERS", id = "127", regex = TRUE)
#     y <- as.markdown(p)
#     if (.Platform$OS.type == "windows") y <- iconv(y, from = encoding(), to = "UTF-8")
#     
#     refdoc <- system.file(package = "polmineR", "fulltext", "reuters_as_markdown.txt")
#     benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
#     benchmark <- paste(benchmark, collapse = "\n")
#     benchmark <- paste(benchmark, "\n", sep = "")
#     
#     expect_identical(nchar(y), nchar(benchmark))
#     expect_identical(y, benchmark)
#     
#     y <- as.character(html(p))
#     y <- gsub("\n\n", "\n", y)
# 
#     refdoc <- system.file(package = "polmineR", "fulltext", "reuters_as_html.html")
#     benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
#     benchmark <- paste(benchmark, collapse = "\n")
#     benchmark <- gsub("\n\n", "\n", benchmark)
#     benchmark <- paste(benchmark, "\n", sep = "")
#     expect_identical(nchar(y), nchar(benchmark))
#     expect_identical(y, benchmark)
#   }
# )
# 
# 
# test_that(
#   "plpr_partition",
#   {
#     skip_on_cran()
#     m <- partition("GERMAPARLMINI", date = "2009-10-28", speaker = "Merkel", regex = TRUE) %>%
#       as.speeches(s_attribute_name = "speaker", s_attribute_date = "date", gap = 100) %>%
#       .[[2]]
#     y <- as.markdown(m)
#     if (.Platform$OS.type == "windows") y <- iconv(y, from = encoding(), to = "UTF-8")
#     
#     refdoc <- system.file(package = "polmineR", "fulltext", "merkel_as_markdown.txt")
#     benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
#     benchmark <- paste(benchmark, collapse = "\n")
#     expect_identical(nchar(y), nchar(benchmark))
#     expect_identical(y, benchmark)
#     
#     y <- as.character(read(m, beautify = TRUE))
#     y <- gsub("\n\n", "\n", y)
# 
#     refdoc <- system.file(package = "polmineR", "fulltext", "merkel_as_html.html")
#     benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
#     benchmark <- paste(benchmark, collapse = "\n")
#     benchmark <- paste(benchmark, "\n", sep = "")
#     benchmark <- gsub("\n\n", "\n", benchmark)
#     expect_identical(nchar(y), nchar(benchmark))
#     expect_identical(y, benchmark)
  }
)

test_that(
  "read",
  {
    skip_on_cran()
    
    # a <- corpus("REUTERS") %>%
    #   subset(places = "argentina") %>%
    #   html(meta = "places") %>%
    #   highlight(lightgreen = "higher") %>%
    #   tooltips(list(lightgreen = "Further information"))
    # 
    # b <- corpus("REUTERS") %>%
    #   subset(places = "argentina") %>%
    #   read(
    #     meta = "places",
    #     highlight = list(lightgreen = "higher"),
    #     tooltips = list(lightgreen = "Further information")
    #   )
    # 
    # expect_identical(a, b)
    # 
    # y <- gsub("\n\n", "\n", b)
    # 
    # refdoc <- system.file(package = "polmineR", "fulltext", "reading_reuters.html")
    # benchmark <- readLines(refdoc, warn = FALSE, encoding = "UTF-8")
    # benchmark <- paste(benchmark, collapse = "\n")
    # benchmark <- paste(benchmark, "\n", sep = "")
    # benchmark <- gsub("\n\n", "\n", benchmark)
    # expect_identical(nchar(y), nchar(benchmark))
    # expect_identical(as.character(y), benchmark)
  }
)
