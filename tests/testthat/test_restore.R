library(polmineR)
testthat::context("restore")
use(pkg = "RcppCWB", corpus = "REUTERS")


test_that(
  "restore RData object",
  {
    k <- kwic("REUTERS", query = "oil")
    rdata_file <- tempfile(fileext = ".RData")
    save(k, file = rdata_file)
    rm(k)
    
    load(rdata_file)
    k <- cp(k) # now it is possible to columns by reference
    enrich(k, s_attribute = "id")
    expect_true("id" %in% colnames(k))
  }
)

test_that(
  "restore rds object",
  {
    k <- kwic("REUTERS", query = "oil")
    kwicfile <- tempfile(fileext = ".rds")
    saveRDS(k, file = kwicfile)
    k2 <- restore(kwicfile)
    enrich(k2, s_attribute = "id")
    expect_true("id" %in% colnames(k2))
  }
)



