## ---- eval = TRUE, message = FALSE, echo = FALSE-------------------------
runcode <- FALSE
if ("CORPUS_REGISTRY" %in% names(Sys.getenv()) && nchar(Sys.getenv("CORPUS_REGISTRY")) > 1){
  library(data.table)
  if (require("rcqp", quietly = T) && require("europarl.en", quietly = T)) runcode <- TRUE
}

## ---- eval = FALSE-------------------------------------------------------
#  Sys.getenv("CORPUS_REGISTRY")

## ---- eval = runcode, message = FALSE------------------------------------
library(polmineR)

## ---- eval = runcode, message = FALSE------------------------------------
use("europarl.en")

## ---- eval = FALSE-------------------------------------------------------
#  install.corpus("europarl.en", repo = "http://polmine.sowi.uni-due.de/packages")

## ---- eval = runcode-----------------------------------------------------
class(polmineR:::CQI)

## ---- eval = FALSE-------------------------------------------------------
#  unlockBinding(env = getNamespace("polmineR"), sym = "CQI")
#  assign("CQI", CQI.rcqp$new(), envir = getNamespace("polmineR"))
#  lockBinding(env = getNamespace("polmineR"), sym = "CQI")
#  }

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("PolMine/polmineR.Rcpp")
#  setCorpusWorkbenchInterface("Rcpp")

## ---- eval = FALSE-------------------------------------------------------
#  unlockBinding(env = getNamespace("polmineR"), sym = "CQI")
#  assign("CQI", CQI.Rcpp$new(), envir = getNamespace("polmineR"))
#  lockBinding(env = getNamespace("polmineR"), sym = "CQI")

## ---- eval = runcode, message = FALSE------------------------------------
corpus()

## ---- eval = FALSE, message = FALSE, results = 'hide'--------------------
#  options()[grep("polmineR", names(options()))]

## ------------------------------------------------------------------------
options("polmineR.left" = 15)
options("polmineR.right" = 15)
options("polmineR.mc" = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("PolMineR/polmineR")

## ---- eval = FALSE-------------------------------------------------------
#  getOption("polmineR.Rcpp")

## ---- eval = FALSE-------------------------------------------------------
#  options("polmineR.Rcpp" = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  kwic("EUROPARL-EN", "Islam")
#  kwic("EUROPARL-EN", "Islam", meta = c("text_date", "speaker_name"))

## ---- eval = FALSE-------------------------------------------------------
#  kwic("EUROPARL-EN", '"Geneva" "Convention"')
#  kwic("EUROPARL-EN", '"[Ss]ocial" "justice"')

## ---- eval = runcode-----------------------------------------------------
count("EUROPARL-EN", "France")
count("EUROPARL-EN", c("France", "Germany", "Britain", "Spain", "Italy", "Denmark", "Poland"))
count("EUROPARL-EN", '"[pP]opulism"')

## ---- eval = runcode, message = FALSE------------------------------------
pop <- dispersion("EUROPARL-EN", "populism", sAttribute = "text_year", progress = FALSE)
popRegex <- dispersion("EUROPARL-EN", '"[pP]opulism"', sAttribute = "text_year", cqp = TRUE, progress = FALSE)

## ---- eval = runcode-----------------------------------------------------
barplot(height = popRegex[,count], names.arg = popRegex[,text_year], las = 2)

## ---- eval = runcode, message = FALSE------------------------------------
br <- cooccurrences("EUROPARL-EN", query = "Brussels")
eu <- cooccurrences("EUROPARL-EN", query = '"European" "Union"', left = 10, right = 10)
subset(eu, rank_ll <= 100)@stat[["word"]][1:15]

## ---- eval = runcode, message = FALSE, results = 'hide'------------------
ep2006 <- partition("EUROPARL-EN", text_year = "2006")

## ---- eval = runcode-----------------------------------------------------
ep2006

## ---- eval = runcode, message = FALSE------------------------------------
barroso <- partition("EUROPARL-EN", speaker_name = "Barroso", regex = TRUE)
sAttributes(barroso, "speaker_name")

## ---- eval = runcode, message = FALSE------------------------------------
ep2002 <- partition("EUROPARL-EN", text_year = "2006")
terror <- cooccurrences(ep2002, "terrorism", pAttribute = "lemma", left = 10, right = 10)

## ---- eval = runcode-----------------------------------------------------
terror@stat[1:10,][,.(lemma, count_partition, rank_ll)]

## ---- eval = runcode-----------------------------------------------------
# one query / one dimension
oneQuery <- dispersion(ep2002, query = 'terrorism', "text_date", progress = FALSE)

# # multiple queries / one dimension
twoQueries <- dispersion(ep2002, query= c("war", "peace"), "text_date", progress = FALSE)

## ---- eval = runcode, message = FALSE------------------------------------
ep2002 <- partition("EUROPARL-EN", text_year = "2002")
ep2002 <- enrich(ep2002, pAttribute = "word")

epPre911 <- partition("EUROPARL-EN", text_year = as.character(1997:2001))
epPre911 <- enrich(epPre911, pAttribute = "word")

F <- features(ep2002, epPre911, included = FALSE)
subset(F, rank_chisquare <= 50)@stat[["word"]]

## ---- eval = FALSE-------------------------------------------------------
#  speakers <- partitionBundle(
#    "EUROPARL-EN", sAttribute = "speaker_id",
#    progress = FALSE, verbose = FALSE
#  )
#  speakers <- enrich(speakers, pAttribute = "word")
#  tdm <- as.TermDocumentMatrix(speakers, col = "count")
#  class(tdm) # to see what it is
#  show(tdm)
#  m <- as.matrix(tdm) # turn it into an ordinary matrix
#  m[c("Barroso", "Schulz"),]

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("plyr")

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("rcqp", repos = "http://polmine.sowi.uni-due.de/packages", type = "win.binary")

## ---- eval = FALSE-------------------------------------------------------
#  install.packages(pkgs = c("htmltools", "htmlwidgets", "magrittr", "iterators", "NLP"))

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("polmineR")

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("PolMine/polmineR", ref = "dev")

## ---- eval = FALSE-------------------------------------------------------
#  Sys.setenv(CORPUS_REGISTRY = "C:/PATH/TO/YOUR/REGISTRY")

## ---- eval = FALSE-------------------------------------------------------
#  library(polmineR)
#  corpus() # to see corpora available at your system

## ---- eval = FALSE-------------------------------------------------------
#  setCorpusWorkbenchInterface("Rcpp")

## ---- eval = runcode-----------------------------------------------------
corpus()

## ----mac_install_polmineR_cran, eval = FALSE-----------------------------
#  install.packages("polmineR")

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools") # unless devtools is already installed
#  devtools::install_github("PolMine/polmineR", ref = "dev")

## ---- eval = FALSE-------------------------------------------------------
#  install.packages(
#    "rcqp",
#    repos = "http://polmine.sowi.uni-due.de/packages",
#    type = "mac.binary"
#    )

## ---- eval = FALSE-------------------------------------------------------
#  install.packages(pkgs = c("RUnit", "devtools", "plyr", "tm"))
#  install.packages("rcqp")

## ---- eval = FALSE-------------------------------------------------------
#  library(polmineR)
#  corpus()

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("RUnit", "devtools", "plyr", "tm")
#  install.packages("rcqp")
#  install.packages("polmineR")

## ---- eval = FALSE-------------------------------------------------------
#  Sys.setenv(CORPUS_REGISTRY = "/PATH/TO/YOUR/REGISTRY/DIRECTORY")
#  
#  # For example the path could look like this:
#  # Sys.setenv(CORPUS_REGISTRY = "/Library/Frameworks/R.framework/Versions/3.3/Resources/library/plprbt/extdata/cwb/registry")

## ------------------------------------------------------------------------
Sys.getenv("CORPUS_REGISTRY")

## ---- eval = FALSE-------------------------------------------------------
#  CORPUS_REGISTRY="/PATH/TO/YOUR/REGISTRY/DIRECTORY"

## ---- eval = FALSE-------------------------------------------------------
#  ?Startup

