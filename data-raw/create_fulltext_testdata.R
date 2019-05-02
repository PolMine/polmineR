# this is the code to generate the test data in the inst/extdata/fulltext folder

library(polmineR)
use("polmineR")
set_template("GERMAPARLMINI")

corpus("GERMAPARLMINI") %>%
  subset(date == "2009-10-28") %>%
  subset(speaker == "Angela Dorothea Merkel") %>%
  as.speeches(s_attribute_name = "speaker", gap = 50) %>%
  .[[2]] %>%
  read() %>%
  as.character() %>%
  cat(file = "~/Lab/github/polmineR/inst/fulltext/merkel_as_html.html")


am <- corpus("GERMAPARLMINI") %>%
  subset(date == "2009-10-28") %>%
  subset(speaker == "Angela Dorothea Merkel") %>%
  as.speeches(s_attribute_name = "speaker", gap = 50) %>%
  .[[2]] %>%
  as.markdown() %>%
  cat(file = "~/Lab/github/polmineR/inst/fulltext/merkel_as_markdown.txt")
