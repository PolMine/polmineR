library(shiny)
library(polmineR)
library(magrittr)
library(DT)
library(shinythemes)

startingTime <- as.character(Sys.time())
fulltext <- ""

if (!"corpora" %in% names(.GlobalEnv)){
  message("... creating environment 'corpora'")
  corpora <- new.env(parent = .GlobalEnv)
}

partitionNames <- c(
  getObjects('partition'),
  getObjects('pressPartition'),
  getObjects('plprPartition')
  )