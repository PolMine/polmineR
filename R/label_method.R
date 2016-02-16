#' assign labels
#' 
#' @param .Object an object of class \code{"partitionBundle"}
#' @param labels labels to assign
#' @param description output
#' @param logfile a file 
#' @param resume logical
#' @param logfile a logfile
#' @examples
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' all <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE, type="plpr")
#' speeches <- as.speeches(all, sAttributeDates="text_date", sAttributeNames="text_name", gap=500)
#' speechesSample <- sample(speeches, 25)
#' df <- label(speechesSample, logfile="/Users/blaette/Lab/tmp/foo.csv")
#' } 
#' @exportMethod label
setGeneric("label", function(.Object, ...) standardGeneric("label"))

setMethod("label", "partitionBundle", function(.Object, labels=c(true=1, false=0), description="Make your choice", logfile=NULL, resume=FALSE, ...){
  retval <- list()
  labels <- c(setNames(as.character(labels), names(labels)), quit="quit")
  for (i in c(1:length(.Object@objects))){
    read(.Object@objects[[i]], ...)
    msg <- paste(
      description, " (",
      paste(labels, names(labels), sep=" = ", collapse=" | "),
      "): ", sep=""
    )
    while (TRUE){
      newLabel <- readline(prompt=msg)
      if (newLabel %in% labels) break
      message("sorry, this is not a valid value, please try again ")
    }
    if (newLabel == "quit") break
    status <- c(
      newLabel,
      names(.Object@objects[i]),
      .Object@objects[[i]]@corpus,
      deparse(.Object[[i]]@sAttributes, control=c("quoteExpressions"))  
    )
    if (!is.null(logfile)){
      cat(paste(paste(status, collapse="\t"), "\n", sep=""), file=logfile, append=TRUE)
    }
    retval[[i]] <- status
  }
  retval <- do.call(rbind, retval)
  colnames(retval) <- c("label", "name", "corpus", "sAttributes")
})

