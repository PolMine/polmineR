#' metainformation
#' 
#' This function is usually part of the procedure to set up a partition. Based
#' on the corpus positions that have initially been set up, a table with all
#' metadata is added to the object, and a list with the attributes and the
#' values that occur.
#' 
#' The values of the metadata are there to determine which subcorpora can be
#' generated. The table with all metadata can be added as it is a basis for the
#' values. It can be omitted for performance and memora reasons.
#' 
#' @param .Object a \code{"partition"} object
#' @param sAttributes a character vector
#' @param struc integer vector, strucs to get metadata for
#' @param ... further parameters
#' @return a \code{"data.frame"}
#' @author Andreas Blaette
#' @rdname meta-method
#' @exportMethod meta
setGeneric("meta", function(.Object, ...) standardGeneric("meta"))

#' @rdname meta-method
setMethod("meta", "character", function(.Object, sAttributes, struc) {
  corpusEncoding <- parseRegistry(.Object)[["encoding"]]
  metaInformation <- sapply(
    sAttributes,
    function(x) {
      retval <- CQI$struc2str(.Object, x, struc)
      Encoding(retval) <- corpusEncoding
      as.utf8(retval)
    })
  names(metaInformation) <- sAttributes
  metaInformation
})



#' @rdname meta-method
setMethod("meta", "partition", function(.Object, sAttributes) {
  if (.Object@xml == "flat") {
    tab <- data.frame(
      lapply(
        sAttributes,
        # USE.NAMES = TRUE,
        function(x) { 
          tmp <- CQI$struc2str(.Object@corpus, x, .Object@strucs)
          Encoding(tmp) <- .Object@encoding
          tmp
        }
      ),
      stringsAsFactors = FALSE
    )
    colnames(tab) <- sAttributes
  } else if (.Object@xml == "nested") {
    # meta <- vapply(sAttributes, FUN.VALUE="character", USE.NAMES=TRUE, function(x)paste(.Object@corpus, '.', x, sep=''))
    tab <- data.frame(
      sapply(
        sAttributes,
        USE.NAMES=TRUE,
        function(x) {
          tmp <- CQI$struc2str(.Object@corpus, x, CQI$cpos2struc(.Object@corpus, x, .Object@cpos[,1]))
          Encoding(tmp) <- .Object@encoding
          tmp
        }
      )
    )
    colnames(tab) <- names(meta)
  }
  return(tab)
})

