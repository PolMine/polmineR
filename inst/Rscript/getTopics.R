#!/usr/bin/env Rscript

library(polmineR)
library(topicmodels)
library(optparse)
library(tm)
library(slam)

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
# dtmTrimmed <- trim(dtmTrimmed, docsToDrop = names(which(slam::row_sums(dtmTrimmed) < 20)))

# dtmTrimmedWeighed <- weigh(dtmTrimmed, method="tfidf")

tmodel <- LDA(
  dtmTrimmed, k=opt$k, method = "Gibbs",
  control = list(burnin = 1000, iter = 1000, keep = 50, verbose=TRUE)
)

saveRDS(tmodel, file=opt$out)

endTime <- Sys.time()
message("END: ", endTime)
message("TIME PASSED: ", format(endTime - startTime))

