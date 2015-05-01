setGeneric("weigh", function(.Object, ...){standardGeneric("weigh")})
#' @exportMethod weigh
#' @docType methods
#' @noRd


setMethod("weigh", "TermDocumentMatrix", function(.Object, method="tfidf"){
  if (method=="tfidf"){
    .Object$v <- .Object$v/col_sums(.Object)[.Object$j] * log2(nDocs(.Object)/row_sums(.Object > 0))[.Object$i]  
    attr(.Object, "weighting") <- c(
      "term frequency - inverse document frequency (normalized)",
      "tf-idf"
    )
  } else if (method=="rel"){
    .Object$v <- .Object$v/col_sums(.Object)[.Object$j]
    attr(.Object, "weighting") <- c(
      "term frequency (normalized)",
      "tf-normalized"
    )
  } else if (method=="rank"){
    warning("not implemented")
  }
  return(.Object)
})



setMethod("weigh", "DocumentTermMatrix", function(.Object, method="tfidf"){
  if (method=="tfidf"){
    .Object$v <- .Object$v/row_sums(.Object)[.Object$i] * log2(nDocs(.Object)/col_sums(.Object > 0))[.Object$j]  
    attr(.Object, "weighting") <- c(
      "term frequency - inverse document frequency (normalized)",
      "tf-idf"
    )
  } else if (method=="rel"){
    .Object$v <- .Object$v/row_sums(.Object)[.Object$i]
    attr(.Object, "weighting") <- c(
      "term frequency (normalized)",
      "tf-normalized"
    )
  } else if (method=="rank"){
    warning("not implemented")
  }
  return(.Object)
})

