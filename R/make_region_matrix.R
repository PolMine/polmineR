#' @include S4classes.R
NULL

setGeneric("make_region_matrix", function(.Object, ...) standardGeneric("make_region_matrix"))

setMethod("make_region_matrix", "subcorpus", function(.Object, s_attribute){
  .Object@cpos
})


setMethod("make_region_matrix", "corpus", function(.Object, s_attribute){
  rng_file <- file.path(.Object@data_dir, paste(s_attribute, "rng", sep = "."))
  rng_file_size <- file.info(rng_file)[["size"]]
  rng_file_con <- file(description = rng_file, open = "rb")
  rng <- readBin(con = rng_file_con, what = integer(), size = 4L, n = rng_file_size, endian = "big")
  close(rng_file_con)
  matrix(rng, ncol = 2L, byrow = TRUE)
})
