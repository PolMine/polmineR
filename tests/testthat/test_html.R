library(polmineR)
use("polmineR")


test_that(
  "enrich partition",
  {
    testthat::skip_on_cran()
    g_min <- corpus("GERMAPARLMINI")
    g_min_sub <- subset(g_min, date == "2009-11-11")
    g_min_sub_speeches <- as.speeches(g_min_sub, s_attribute_name = "speaker", s_attribute_date = "date", progress = FALSE)
    output <- html(g_min_sub_speeches, progress = TRUE, beautify = FALSE)
    expect_equal(is(output)[1], "html")
    }
)


test_that(
  "html() and show_info() render markdown as HTML (#257)",
  {
    testthat::skip_on_cran()
    sp <- corpus("GERMAPARLMINI") %>%
      subset(date == "2009-11-11") %>%
      as.speeches(s_attribute_name = "speaker", s_attribute_date = "date", progress = FALSE)
    doc <- as.character(html(sp[[1]], beautify = FALSE))
    expect_match(doc, "<h3[^>]*>Norbert Lammert \\| 2009-11-11 \\| NA</h3>")
    expect_match(doc, '<p><span id="80450" token="Die" class="fulltext">Die</span>', fixed = TRUE)
    expect_no_match(doc, "### ", fixed = TRUE)

    use(pkg = "RcppCWB", corpus = "REUTERS")
    old <- options(viewer = function(...) invisible(NULL))
    info <- as.character(show_info(corpus("REUTERS")))
    options(old)
    expect_match(info, '<h1[^>]*>REUTERS corpus</h1>')
    expect_no_match(info, "<html", fixed = TRUE)
  }
)
