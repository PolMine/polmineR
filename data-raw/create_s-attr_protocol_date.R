library(data.table)
library(cwbtools)

regdir <- "/Users/andreasblatte/Lab/github/polmineR/inst/extdata/cwb/registry"
datadir <- "/Users/andreasblatte/Lab/github/polmineR/inst/extdata/cwb/indexed_corpora/germaparlmini"

s_attr_date <- RcppCWB::s_attribute_decode(
  corpus = "GERMAPARLMINI",
  s_attribute = "date",
  data_dir = datadir,
  method = "R"
)

dt <- data.table(s_attr_date)

dt_min <- dt[, {list(cpos_left = min(.SD$cpos_left), cpos_right = max(.SD$cpos_right))},  by = "value"]

s_attribute_encode(
  values = dt_min$value,
  data_dir = datadir,
  s_attribute = "protocol_date",
  corpus = "GERMAPARLMINI",
  region_matrix = as.matrix(dt_min[, c("cpos_left", "cpos_right")]),
  registry_dir = regdir,
  encoding = "latin1",
  method = "R"
)
