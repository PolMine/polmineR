#!/usr/bin/env Rscript

library(polmineR)
library(polmineR.topics)
library(parallel)
library(magrittr)
library(optparse)

# usage:
# Rscript getTopics.R -f /path/to/input/filename.RData -o /path/to/output/filename.RData

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL,  help="input file name", metavar="character"),
  make_option(c("-o", "--out"), type="character", default=NULL, help="output file name", metavar="character"),
  make_option(c("-k", "--k"), type="numeric", default=50, help="number of topics to be computed", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$file)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

startTime <- Sys.time()
message("START: ", startTime)

dtmTrimmed <- readRDS(file=opt$file)
ldas <- ldaBundle(dtmTrimmed, ks=opt$k, mc=1, verbose=TRUE)
saveRDS(ldas, file=opt$out)

endTime <- Sys.time()
message("END: ", endTime)
message("TIME PASSED: ", format(endTime - startTime))

