#' session class
#' 
#' default settings for current session
#' 
#' @slot project currently active project 
#' @slot defaultCorpus corpus to use if none is provided
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
#' @slot consoleEncoding default console encoding
#' @slot smtpServer default server to use for sending mail
#' @slot smtpPort default port 
#' @slot email default email address
#' @slot partitionDir default partition directory
#' @slot defaultKwicCollocate default collocate for kwic display
#' @slot defaultKwicNode default node for kwic analysis
#' @exportClass session
#' @rdname session
#' @aliases session
setClass(
  "session",
  slots=c(
    project="character",
    defaultCorpus="character",
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
    consoleEncoding="character",
    smtpServer="character",
    smtpPort="character",
    email="character",
    partitionDir="character",
    defaultKwicCollocate="character",
    defaultKwicNode="character"
    ))

#' @export
session <- new(
  "session",
  project=c(""),
  defaultCorpus="PLPRBTTXT",
  pAttribute="word",
  leftContext=5,
  rightContext=5,
  minSignificance=3.84,
  minFrequency=5,
  posFilter=c("NN"),
  filterType="include",
  kwicMetadata=c("text_party"),
  kwicNo=10,
  metadata = c("text_party", "text_speaker", "text_date"),
  multicore=TRUE,
  consoleEncoding="UTF-8",
  smtpServer="mailout.uni-due.de",
  smtpPort="587",
  email="polmine@uni-due.de",
  partitionDir="/Users/blaette/Lab/tmp/partitions",
  defaultKwicCollocate="",
  defaultKwicNode="Suche"           
  )

setGeneric("project<-", function(x, value) standardGeneric("project<-"))

#' @param x bla
#' @param object bla
#' @param project bla
#' @param value to assign
#' @rdname session
setReplaceMethod("project", signature=c(x="session", value="character"), function(x, value) {
  x@project <- value
  x
  })

setGeneric("project", function(object) standardGeneric("project"))

#' @rdname session
setMethod("project", "session", function(object) print(object@project))

#' @rdname session
setMethod("project", "missing", function(){
  list.dirs(Sys.getenv("POLMINER_DIR"), full.names=FALSE, recursive=FALSE)
})




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