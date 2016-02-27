#' session class
#' 
#' default settings for current session
#' 
#' @slot project currently active project
#' @slot projectDir directory of the current project 
#' @slot corpus corpus to use if none is provided
#' @slot pAttribute default pAttribute
#' @slot left default left context
#' @slot right default right context
#' @slot minSignificance default minimum significance
#' @slot minFrequency default minimum frequency
#' @slot posFilter default filter for POS
#' @slot filterType default filter type
#' @slot lineview logical
#' @slot meta default metadata 
#' @slot multicore default multicore
#' @slot cores number of CPU cores to use
#' @slot consoleEncoding default console encoding
#' @slot smtpServer default server to use for sending mail
#' @slot smtpPort default port 
#' @slot email default email address
#' @slot webDir directory for putting files that shall be accessible by www
#' @slot partitionDir default partition directory
#' @slot defaultKwicCooccurrence default coccurrence for kwic display
#' @slot defaultKwicNode default node for kwic analysis
#' @slot defaultRegistry the COPRUS_REGISTRY used when loading polmineR
#' @slot browse logical, whether to browse results
#' @slot backend parallel backend (doMC, doSNOW etc.)
#' @param project the project
#' @param object a session class object
#' @param x an object
#' @param value character string to be assigned
#' @exportClass session
#' @rdname session
#' @aliases session email filterType kwicNo left metadata minFrequency 
#' minSignificance multicore pAttribute posFilter project right smtpPort smtpServer
#' corpus corpus<- email<- filterType<- 
#' kwicNo<- left<- metadata<- minFrequency<-
#' minSignificance<- multicore<- pAttribute<- posFilter<-
#' project<- right<- smtpPort<- smtpServer<-
#' cores cores<-
setClass(
  "session",
  slots=c(
    project="character",
    projectDir="character",
    corpus="character",
    pAttribute="character",
    left="numeric",
    right="numeric",
    lineview="logical",
    minSignificance="numeric",
    minFrequency="numeric",
    posFilter="character",
    filterType="character",
    meta="character",
    multicore="logical",
    cores="numeric",
    consoleEncoding="character",
    smtpServer="character",
    smtpPort="character",
    email="character",
    webDir="character",
    partitionDir="character",
    defaultKwicNode="character",
    defaultRegistry="character",
    browse="logical",
    kwicLineView="logical",
    backend="character"
    ))

#' @export session
session <- new(
  "session",
  project=c(""),
  projectDir=c(""),
  corpus="PLPRBTTXT",
  pAttribute="word",
  left=5,
  right=5,
  minSignificance=3.84,
  minFrequency=5,
  filterType="include",
  lineview=FALSE,
  meta = as.character(c()),
  multicore=TRUE,
  cores=2,
  consoleEncoding="UTF-8",
  smtpServer="mailout.uni-due.de",
  smtpPort="587",
  email="polmine@uni-due.de",
  webDir="",
  partitionDir="",
  defaultKwicNode="Suche",
  browse=FALSE,
  backend="doSNOW"
  )





#' @rdname session
createProject <- function(project){
  newProjectDir <- file.path(
    Sys.getenv("POLMINER_DIR"),
    project
  )
  if (newProjectDir %in% list.files(Sys.getenv("POLMINER_DIR"))){
   warning("the directory to be created already exists") 
  } else {
    message("... creating ", newProjectDir)
    dir.create(newProjectDir)
    for (subdir in c("partition", "partitionBundle", "context", "kwic", "comp")){
      newDir <- file.path(newProjectDir, subdir)
      message("... creating ", newDir)
      dir.create(newDir)
    }
  }  
}
