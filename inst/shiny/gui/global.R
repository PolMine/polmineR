library(shiny)
library(polmineR)
library(magrittr)
library(DT)
library(shinythemes)


partitionNames <- c(
  getObjects('partition'),
  getObjects('pressPartition'),
  getObjects('plprPartition')
  )