library(shiny)
library(polmineR)
# library(polmineR.shiny)
library(magrittr)
loadNamespace("DT")
library(shinythemes)

source("modules/cooccurrences.R")
source("modules/dispersion.R")
source("modules/partition.R")
source("modules/settings.R")
source("modules/corpus.R")
source("modules/features.R")
source("modules/utils.R")
source("modules/count.R")
source("modules/kwic.R")
source("modules/read.R")

values <- reactiveValues()
values[["partitions"]] <- list()
values[["corpora"]] <- list()
values[["fulltext"]] <- ""
values[["startingTime"]] <- as.character(Sys.time())

debug <- TRUE
