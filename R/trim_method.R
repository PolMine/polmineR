#' @include partition_class.R partitionBundle_class.R context_class.R contextBundle_class.R
#' @include features_class.R
NULL

#' trim an object
#' 
#' Method to trim and adjust objects by 
#' applying thresholds, minimum frequencies etc. It can be applied to 'context',
#' 'features', 'context', 'partition' and 'partitionBundle' objects.
#' 
#' @param object the object to be trimmed
#' @param termsToKeep ...
#' @param termsToDrop ...
#' @param docsToKeep ...
#' @param docsToDrop ...
#' @param verbose logical
#' @param ... further arguments
#' @author Andreas Blaette
#' @docType methods
#' @aliases trim trim-method trim,TermDocumentMatrix-method
#' @rdname trim-method
setGeneric("trim", function(object, ...){standardGeneric("trim")})



#' @importFrom Matrix rowSums
#' @importFrom tm stopwords
#' @importFrom slam as.simple_triplet_matrix
#' @rdname trim-method
setMethod("trim", "TermDocumentMatrix", function(object, termsToKeep=NULL, termsToDrop=NULL, docsToKeep=NULL, docsToDrop=NULL, verbose=TRUE){
  .rmBlank <- function(mat, verbose=TRUE){
    if (verbose==TRUE) message("... removing empty rows")
    matTmp <- as.sparseMatrix(mat)
    matTmp <- matTmp[which(rowSums(matTmp) > 0),]
    mat <- as.simple_triplet_matrix(matTmp)
    class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
    mat
  }
  if (!is.null(docsToKeep)){
    object <- object[,which(colnames(object) %in% docsToKeep)]
  }
  if (!is.null(docsToDrop)){
    object <- object[,which(!colnames(object) %in% docsToDrop)]
  }
  if (!is.null(termsToKeep)){
    object <- object[which(rownames(object) %in% termsToKeep),]
  }
  if (!is.null(termsToDrop)){
    object <- object[which(!rownames(object) %in% termsToDrop), ]
  }
  object
})

#' @rdname trim-method
setMethod("trim", "DocumentTermMatrix", function(object, ...){
  t(trim(t(object), ...))
})


#' trim dispersion object
#' 
#' Drop unwanted columns in a dispersion object, and merge columns by either explicitly stating the columns,
#' or providing a regex. If merge$old is length 1, it is assumed that a regex is provided
#' 
#' @param object a crosstab object to be adjusted
#' @param drop defaults to NULL, or a character vector giving columns to be dropped 
#' @param merge a list giving columns to be merged or exactly one string with a regex (see examples)
#' @return a modified crosstab object
#' @docType methods
#' @rdname dispersion-class
#' @exportMethod trim
#' @docType methods
setMethod("trim", "dispersion", function(object, drop=NULL, merge=list(old=c(), new=c())){
  if (!is.null(drop)){
    object <- .crosstabDrop(x=object, filter=drop, what="drop")
  }
  if (!all(sapply(merge, is.null))){
    if (length(merge$new) != 1) warning("check length of character vectors in merge-list (needs to be 1)")
    if (length(merge$old) == 2){
      object <- .crosstabMergeCols(
        object,
        colnameOld1=merge$old[1], colnameOld2=merge$old[2],
        colnameNew=merge$new[1]
      )
    } else if (length(merge$old == 1 )){
      object <- .crosstabMergeColsRegex(object, regex=merge$old[1], colname.new=merge$new[1])
    } else {
      warning("length of merge$old not valid")
    }
  }
})


#' @rdname context-class
setMethod("trim", "context", function(object, sAttribute = NULL, verbose = TRUE, progress = TRUE){
  
  if(!is.null(sAttribute)){
    stopifnot(length(sAttribute) == 1)
    sAttrCol <- paste(sAttribute, "int", sep = "_")
    if (!sAttrCol %in% colnames(object@cpos)){
      object <- enrich(object, sAttribute = sAttribute)
    }
    setnames(object@cpos, old = sAttrCol, new = "struc")
    
    position <- 0 # work around to make data.table syntax pass R CMD check
    struc <- 0 # work around to make data.table syntax pass R CMD check
    
    if (verbose) message("... checking boundaries of regions")
    if (progress) pb <- txtProgressBar(min = 1, max = object@count, style = 3)
    .checkBoundary <- function(.SD, .GRP){
      if (progress) setTxtProgressBar(pb, value = .GRP)
      struc_hit <- .SD[position == 0][["struc"]][1]
      .SD[struc == struc_hit]
    }
    object@cpos <- object@cpos[, .checkBoundary(.SD, .GRP), by = "hit_no"]
    close(pb)
    setnames(object@cpos, old = "struc", new = sAttrCol)
  }
  
  object
})

# #' @rdname cooccurrences-class
# setMethod("trim", "cooccurrences", function(object, mc=TRUE, reshape=FALSE, by=NULL, ...){
#   if (reshape == TRUE) object <- .reshapeCooccurrences(object, mc=mc)
#   if (is.null(by) == FALSE){
#     if (class(by) %in% c("compCooccurrences", "cooccurrencesReshaped")){
#       bidirectional <- strsplit(rownames(by@stat), "<->")
#       fromTo <- c(
#         sapply(bidirectional, function(pair) paste(pair[1], "->", pair[2], sep="")),
#         sapply(bidirectional, function(pair) paste(pair[2], "->", pair[1], sep=""))
#       ) 
#       object@stat <- object@stat[which(rownames(object@stat) %in% fromTo),]
#     }
#   }
#   callNextMethod()
# })
# 

