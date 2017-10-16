# library(polmineR)
# testthat::context("encode")
# 
# test_that(
#   "encode",
#   {
#     if (getOption("polmineR.cwb-encode") && getOption("polmineR.cwb-s-encode")){
#       library(tm)
#       reut21578 <- system.file("texts", "crude", package = "tm")
#       reuters.tm <- VCorpus(DirSource(reut21578), list(reader = readReut21578XMLasPlain))
# 
#       library(tidytext)
#       reuters.tibble <- tidy(reuters.tm)
#       reuters.tibble[["topics_cat"]] <- sapply(
#         reuters.tibble[["topics_cat"]],
#         function(x) paste(reuters.tibble[["topics_cat"]], collapse = "|")
#       )
#       reuters.tibble[["places"]] <- sapply(
#         reuters.tibble[["places"]],
#         function(x) paste(x, collapse = "|")
#       )
#       reuters.tidy <- unnest_tokens(
#         reuters.tibble, output = "word", input = "text", to_lower = FALSE
#       )
# 
#       tmpCwbDir <- tempdir()
#       registryDir <- file.path(tmpCwbDir, "registry")
#       dir.create(registryDir)
#       dir.create(file.path(tmpCwbDir, "indexed_corpora"))
#       resetRegistry(registryDir = registryDir)
# 
#       encode(reuters.tidy, name = "reuters", sAttributes = c("language", "places"))
#       corpus()
#       expect_equal(size("REUTERS"), 4050)
#       expect_equal(pAttributes("REUTERS"), "word")
#     }
#   }
# )
# 
