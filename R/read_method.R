#' read an object
#' 
#' A html page will be generated and displayed in the viewer pane of RStudio.
#' 
#' @param .Object an object to be read
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
setGeneric("read", function(.Object, ...) standardGeneric("read"))

#' @rdname read-method
setMethod("read", "plprPartition", function(.Object, meta=c("text_name", "text_party", "text_date")){
  fulltextHtml <- html(.Object, meta=meta)
  if(require("htmltools", quietly = TRUE)){
    htmltools::html_print(fulltextHtml)  
  } else {
    warning("package htmltools required, but not available")
  }
})

#' @rdname read-method
setMethod("read", "pressPartition", function(.Object, meta=c("text_newspaper", "text_date")){
  fulltextHtml <- html(.Object, meta=meta)
  if(require("htmltools", quietly = TRUE)){
    htmltools::html_print(fulltextHtml)  
  } else {
    warning("package htmltools required, but not available")
  }
})
