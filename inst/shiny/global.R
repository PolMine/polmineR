library(polmineR)
library(data.table)
loadNamespace("DT")

check_and_install_pkg <- function(pkgname){
  if (isFALSE(pkgname %in% rownames(installed.packages()))){
    message(sprintf("The '%s' package is required to use the polmineR shiny app, ", pkgname),
            "but it is not yet installed. Do you want to install it now?")
    answer <- utils::menu(choices = c("Yes", "No"))
    if (answer == 1L){
      install.packages(pkgname)
    } else {
      stop(
        sprintf("Aborting - the polmineR shiny app requires that the %s package is installed.", pkgname)
      )
    }
  }
}
check_and_install_pkg("shiny")
check_and_install_pkg("shinythemes")
library(shiny)
library(shinythemes)

options(polmineR.shiny = TRUE)
options(polmineR.buttons = TRUE)

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
