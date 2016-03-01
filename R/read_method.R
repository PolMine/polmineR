#' Return to the original text and read
#' 
#' Generate text (i.e. html) and read it in the viewer pane of RStudio. If called on
#' a \code{"partitionBundle"}-object, skip through the partitions contained in the
#' bundle.
#' 
#' @param .Object an object to be read (\code{"partition" or "partitionBundle"})
#' @param meta a character vector supplying s-attributes for the metainformation
#'   to be printed, if not stated explicitly, session settings will be used
#' @param regex a list of character vectors with regular expressions to
#'   highlight relevant terms or expressions; the names of the list provide the
#'   colors (see examples)
#' @param cqp a list of character vectors with regular expressions to
#'   highlight relevant terms or expressions; the names of the list provide the
#'   colors
#' @param ... further parameters passed into read
#' @exportMethod read
#' @rdname read-method
#' @examples
#' use("polmineR.sampleCorpus")
#' merkel <- partition(
#'   "PLPRBTTXT",
#'   list(text_date="2009-11-10", text_name="Angela Dorothea Merkel"),
#'   type="plpr"
#' )
#' read(merkel, meta=c("text_name", "text_date"))
#' read(
#'   merkel,
#'   highlight=list(yellow=c("Deutschland", "Bundesrepublik"), lightgreen="Regierung")
#'   )
#' \dontrun{
#' all <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE, type="plpr")
#' speeches <- as.speeches(all, sAttributeDates="text_date", sAttributeNames="text_name", gap=500)
#' read(speeches)
#' 
#' migVocab <- count(speeches, query=c("Migration", "Integration", "Zuwanderung"))
#' read(migVocab, speeches, col="Integration")
#' }
#' @seealso For concordances / a keword-in-context display, see \code{\link{kwic}}.
setGeneric("read", function(.Object, highlight, ...) standardGeneric("read"))

#' @rdname read-method
setMethod("read", signature("partition", "list"), function(.Object, highlight=list(), cqp=FALSE, meta=NULL, verbose=TRUE, ...){
  if (is.null(meta)){
    if (all(session@meta %in% sAttributes(.Object@corpus))) {
      meta <- slot(get('session', '.GlobalEnv'), 'meta')
      if (verbose == TRUE) message("... using meta from session: ", meta)
    }
  }
  fulltextHtml <- html(.Object, meta=meta, highlight=highlight, cqp=cqp, ...)
  if(require("htmltools", quietly = TRUE)){
    htmltools::html_print(fulltextHtml)  
  } else {
    warning("package htmltools required, but not available")
  }
})

#' @rdname read-method
setMethod("read", signature("partitionBundle", "list"), function(.Object, highlight=list(), cqp=FALSE, ...){
  for (i in c(1:length(.Object@objects))){
    read(.Object@objects[[i]], highlight=highlight, cqp=cqp, ...)
    key <- readline("Enter 'q' to quit, any other key to continue. ")
    if (key == "q") break
  }
})

#' rdname read-method
setMethod("read", "data.table", function(.Object, highlight=list(), cqp=FALSE, partitionBundle, col, ...){
  stopifnot(col %in% colnames(.Object))
  DT <- .Object[which(.Object[[col]] > 0)]
  partitionsToGet <- DT[["partition"]]
  toRead <- as.bundle(lapply(partitionsToGet, function(x) partitionBundle@objects[[x]]))
  read(toRead, highlight=list(yellow=col), cqp=cqp, ...)
})