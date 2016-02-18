#' Return to the original text and read
#' 
#' Generate text (i.e. html) and read it in the viewer pane of RStudio. If called on
#' a \code{"partitionBundle"}-object, skip through the partitions contained in the
#' bundle.
#' 
#' @param .Object an object to be read (\code{"partition" or "partitionBundle"})
#' @param meta a character vector supplying s-attributes for the metainformation
#'   to be printed, if not stated explicitly, session settings will be used
#' @param highlight a list of character vectors with regular expressions to
#'   highlight relevant terms or expressions; the names of the list provide the
#'   colors (see examples)
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
#' read(merkel)
#' read(
#'   merkel,
#'   highlight=list(yellow=c("Deutschland", "Bundesrepublik"), lightgreen="Regierung")
#'   )
#' \dontrun{
#' all <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE, type="plpr")
#' speeches <- as.speeches(all, sAttributeDates="text_date", sAttributeNames="text_name", gap=500)
#' read(speeches)
#' }
#' @seealso For concordances / a keword-in-context display, see \code{\link{kwic}}.
setGeneric("read", function(.Object, ...) standardGeneric("read"))

#' @rdname read-method
setMethod("read", "partition", function(.Object, meta=NULL, highlight=list(), verbose=TRUE, ...){
  if (is.null(meta)){
    parsedRegistry <- parseRegistry(.Object@corpus)
    if ("meta" %in% names(parsedRegistry)){
      meta <- parsedRegistry[["meta"]]
    } else {
      if (verbose == TRUE) message("... no default metadata stated as corpus properties in registry, trying to use session settings")
      if (all(session@metadata %in% sAttributes(.Object@corpus))){
        meta <- session@metadata
      } else {
        stop("metadata not available, please set session settings or indicate explicitly")
      }
    }
  }
  fulltextHtml <- html(.Object, meta=meta, highlight=highlight, ...)
  if(require("htmltools", quietly = TRUE)){
    htmltools::html_print(fulltextHtml)  
  } else {
    warning("package htmltools required, but not available")
  }
})

#' @rdname read-method
setMethod("read", "partitionBundle", function(.Object, ...){
  for (i in c(1:length(.Object@objects))){
    read(.Object@objects[[i]], ...)
    key <- readline("Enter 'q' to quit, any other key to continue. ")
    if (key == "q") break
  }
})