#' @include S4classes.R
NULL

#' Apply Weight to Matrix
#' 
#' @param .Object the matrix to be weighed
#' @param method the kind of weight to apply
#' @param ... further parameters
#' @exportMethod weigh
#' @docType methods
#' @rdname weigh-method
#' @name weigh
setGeneric("weigh", function(.Object, ...) standardGeneric("weigh") )

#' @importFrom slam row_sums col_sums
#' @importFrom tm nDocs
#' @rdname weigh-method
setMethod("weigh", "TermDocumentMatrix", function(.Object, method = "tfidf"){
  if (method == "tfidf"){
    .Object$v <- .Object$v/col_sums(.Object)[.Object$j] * log2(nDocs(.Object)/row_sums(.Object > 0))[.Object$i]  
    attr(.Object, "weighting") <- c(
      "term frequency - inverse document frequency (normalized)",
      "tf-idf"
    )
  } else if (method == "rel"){
    .Object$v <- .Object$v / col_sums(.Object)[.Object$j]
    attr(.Object, "weighting") <- c(
      "term frequency (normalized)",
      "tf-normalized"
    )
  } else if (method == "rank"){
    warning("not implemented")
  }
  return(.Object)
})


#' @rdname weigh-method
setMethod("weigh", "DocumentTermMatrix", function(.Object, method = "tfidf"){
  if (method=="tfidf"){
    .Object$v <- .Object$v/row_sums(.Object)[.Object$i] * log2(nDocs(.Object)/col_sums(.Object > 0))[.Object$j]  
    attr(.Object, "weighting") <- c(
      "term frequency - inverse document frequency (normalized)",
      "tf-idf"
    )
  } else if (method == "rel"){
    .Object$v <- .Object$v/row_sums(.Object)[.Object$i]
    attr(.Object, "weighting") <- c(
      "term frequency (normalized)",
      "tf-normalized"
    )
  } else if (method == "rank"){
    warning("not implemented")
  }
  return(.Object)
})

