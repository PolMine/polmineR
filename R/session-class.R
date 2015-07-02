#' session class
#' 
#' default settings for current session
#' 
#' @slot project currently active project
#' @slot projectDir directory of the current project 
#' @slot corpus corpus to use if none is provided
#' @slot pAttribute default pAttribute
#' @slot leftContext default left context
#' @slot rightContext default right context
#' @slot minSignificance default minimum significance
#' @slot minFrequency default minimum frequency
#' @slot posFilter default filter for POS
#' @slot filterType default filter type
#' @slot kwicMetadata default metadata displayed for kwic
#' @slot kwicNo default number of kwic lines
#' @slot metadata default metadata 
#' @slot multicore default multicore
#' @slot cores number of CPU cores to use
#' @slot consoleEncoding default console encoding
#' @slot smtpServer default server to use for sending mail
#' @slot smtpPort default port 
#' @slot email default email address
#' @slot webDir directory for putting files that shall be accessible by www
#' @slot partitionDir default partition directory
#' @slot defaultKwicCollocate default collocate for kwic display
#' @slot defaultKwicNode default node for kwic analysis
#' @slot defaultRegistry the COPRUS_REGISTRY used when loading polmineR
#' @param project the project
#' @param object a session class object
#' @param x an object
#' @param value character string to be assigned
#' @exportClass session
#' @rdname session
#' @aliases session email filterType kwicMetadata kwicNo leftContext metadata minFrequency 
#' minSignificance multicore pAttribute posFilter project rightContext smtpPort smtpServer
#' corpus corpus<- email<- filterType<- kwicMetadata<-
#' kwicNo<- leftContext<- metadata<- minFrequency<-
#' minSignificance<- multicore<- pAttribute<- posFilter<-
#' project<- rightContext<- smtpPort<- smtpServer<-
setClass(
  "session",
  slots=c(
    project="character",
    projectDir="character",
    corpus="character",
    pAttribute="character",
    leftContext="numeric",
    rightContext="numeric",
    minSignificance="numeric",
    minFrequency="numeric",
    posFilter="character",
    filterType="character",
    kwicMetadata="character",
    kwicNo="numeric",
    metadata="character",
    multicore="logical",
    cores="numeric",
    consoleEncoding="character",
    smtpServer="character",
    smtpPort="character",
    email="character",
    webDir="character",
    partitionDir="character",
    defaultKwicCollocate="character",
    defaultKwicNode="character",
    defaultRegistry="character"
    ))

#' @export session
session <- new(
  "session",
  project=c(""),
  projectDir=c(""),
  corpus="PLPRBTTXT",
  pAttribute="word",
  leftContext=5,
  rightContext=5,
  minSignificance=3.84,
  minFrequency=5,
  posFilter=c("NN"),
  filterType="include",
  kwicMetadata=c("text_party"),
  kwicNo=10,
  metadata = c("text_party", "text_name", "text_date"),
  multicore=TRUE,
  cores=2,
  consoleEncoding="UTF-8",
  smtpServer="mailout.uni-due.de",
  smtpPort="587",
  email="polmine@uni-due.de",
  webDir="",
  partitionDir="",
  defaultKwicCollocate="",
  defaultKwicNode="Suche"           
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
    for (subdir in c("partition", "partitionCluster", "context", "kwic", "keyness")){
      newDir <- file.path(newProjectDir, subdir)
      message("... creating ", newDir)
      dir.create(newDir)
    }
  }  
}