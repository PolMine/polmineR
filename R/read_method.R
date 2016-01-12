#' read an object
#' 
#' A html page will be generated and displayed in the viewer pane of RStudio.
#' 
#' @param .Object an object to be read
#' @param meta a character vector indicating the s-attributes for the metainformation to be printed
#' @param highlight a list of character vectors with regular expressions
#' to highlight relevant terms or expressions; the names of the list provide the colors (see examples)
#' @param ... further parameters
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
setGeneric("read", function(.Object, ...) standardGeneric("read"))

#' @rdname read-method
setMethod("read", "partition", function(.Object, meta=NULL, highlight=list()){
  fulltextHtml <- html(.Object, meta=meta, highlight=highlight)
  if(require("htmltools", quietly = TRUE)){
    htmltools::html_print(fulltextHtml)  
  } else {
    warning("package htmltools required, but not available")
  }
})

#' @rdname read-method
setMethod("read", "plprPartition", function(.Object, meta=c("text_name", "text_party", "text_date"), highlight=list()){
  fulltextHtml <- html(.Object, meta=meta, highlight=highlight)
  if(require("htmltools", quietly = TRUE)){
    htmltools::html_print(fulltextHtml)  
  } else {
    warning("package htmltools required, but not available")
  }
})

#' @rdname read-method
setMethod("read", "pressPartition", function(.Object, meta=c("text_newspaper", "text_date"), highlight=list()){
  fulltextHtml <- html(.Object, meta=meta, highlight=highlight)
  if(require("htmltools", quietly = TRUE)){
    htmltools::html_print(fulltextHtml)  
  } else {
    warning("package htmltools required, but not available")
  }
})
