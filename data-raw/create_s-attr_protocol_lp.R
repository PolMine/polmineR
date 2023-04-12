library(data.table)
library(cwbtools)

regdir <- "~/Lab/github/polmineR/inst/extdata/cwb/registry"
datadir <- "~/Lab/github/polmineR/inst/extdata/cwb/indexed_corpora/germaparlmini"

s_attr_lp <- RcppCWB::s_attribute_decode(
  corpus = "GERMAPARLMINI",
  s_attribute = "protocol_date",
  data_dir = datadir,
  method = "R"
)

s_attr_lp$value <- rep("17", times = nrow(s_attr_lp))

s_attribute_encode(
  values = s_attr_lp$value,
  data_dir = datadir,
  s_attribute = "protocol_lp",
  corpus = "GERMAPARLMINI",
  region_matrix = as.matrix(s_attr_lp[, c("cpos_left", "cpos_right")]),
  registry_dir = regdir,
  encoding = "latin1",
  method = "R"
)
