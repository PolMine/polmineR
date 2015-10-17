
#' merge two columns in a crosstab object
#' 
#' The columns with absolute frequencies (partition size, frequencies for a
#' query) are merged, and the relative frequencies are recalculated.
#' 
#' @method mergeCols crosstab
#' @param object the partition object
#' @param colnameOld1 the colname of the first column to be merged
#' @param colnameOld2 the colname of the second column to be merged
#' @param colnameNew the colname of the merged column
#' @return the returned crosstab object has a matrix with partition sizes,
#' absoute query frequencies and relative query frequencies, just as the input
#' @noRd
.crosstabMergeCols <- function(object, colnameOld1, colnameOld2, colnameNew) {
  object@objects[,colnameOld1] <- object@objects[,colnameOld1] + object@objects[,colnameOld2]
  colnames(object@objects)[which(colnames(object@objects)==colnameOld1)] <- colnameNew
  object@objects <- .dropcols(object@objects, colnameOld2)
  object@abs[,colnameOld1] <- object@abs[,colnameOld1] + object@abs[,colnameOld2]
  colnames(object@abs)[which(colnames(object@abs)==colnameOld1)] <- colnameNew
  object@abs <- object@abs[-grep(colnameOld2, colnames(object@abs))]
  object@rel <- object@abs/object@objects
  object
}

#' merge columns that match a regex
#' 
#' Merge columns of a crosstab object that match a regular expression
#' 
#' @param object the partition object
#' @param regex a regular expression
#' @param colname.new the colname of the merged column
#' @return a crosstab object has a matrix with partition sizes,
#' absoute query frequencies and relative query frequencies, just as the input
#' @noRd
.crosstabMergeColsRegex <- function(object, regex, colname.new) {
  match <- grep(regex, colnames(object@objects))
  message('...', length(match), 'columns to be merged')
  if (length(match)>1) {
    object@objects <- cbind(object@objects, rowSums(object@objects[,match]))
    object@objects <- .dropcols(object@objects, regex)      
    colnames(object@objects)[ncol(object@objects)] <- colname.new
    object@abs <- cbind(object@abs, rowSums(object@abs[,match]))
    object@abs <- .dropcols(object@abs, regex)      
    colnames(object@abs)[ncol(object@abs)] <- colname.new
  } else if (length(match==1)) {
    object@objects <- cbind(object@objects, object@objects[,match])
    object@objects <- .dropcols(object@objects, regex)      
    colnames(object@objects)[ncol(object@objects)] <- colname.new
    object@abs <- cbind(object@abs, object@abs[,match])
    object@abs <- .dropcols(object@abs, regex)      
    colnames(object@abs)[ncol(object@abs)] <- colname.new
  } else {
    object@objects <- cbind(object@objects, rep(0, times=nrow(object@objects)))
    colnames(object@objects)[ncol(object@objects)] <- colname.new    
    object@abs <- cbind(object@abs, rep(0, times=nrow(object@abs)))
    colnames(object@abs)[ncol(object@abs)] <- colname.new
  }
  object@rel <- object@abs/object@objects
  object
}


#' drop columns from a crosstab object
#' 
#' Columns indicated in a character vector are either dropped or maintained,
#' depending on whether the vector is used as a stoplist or a list of columns
#' to be kept
#' 
#' @param x the crosstab object
#' @param filter a character vector with colnames
#' @param what if "drop", cols is used as a stoplist, if "keep", itis a list with
#' the columns to be kept
#' @return you get a crosstab object with partition size, absolute and relative
#' frequencies
#' @noRd
.crosstabDrop <- function(x, filter, what="drop"){
  object <- x
  if (what=="drop"){
    object@objects <- .dropcols(object@objects, filter)
    object@abs <- .dropcols(object@abs, filter)
    object@rel <- .dropcols(object@rel, filter)
  } else if (what=="keep"){
    object@objects <- object@objects[,which(colnames(object@objects) %in% filter)]
    object@abs <- object@abs[,which(colnames(object@abs) %in% filter)]
    object@rel <- object@rel[,which(colnames(object@rel) %in% filter)]
  }
  object
}


.dropcols <- function(tab, colname) {
  drop <- grep(colname, colnames(tab))
  tab <- tab[-drop]
  tab
}


# documented with crosstab class
#' @docType methods
#' @rdname dispersion-class
setMethod("t", "dispersion", function(x){
  y <- x
  y@sizes <- t(x@sizes)
  y@abs <- x@abs
  y@rel <- t(x@rel)
  y@dim <- rev(x@dim)
  y  
})

#' @exportMethod show
#' @docType methods
#' @rdname dispersion-class
setMethod("show", "dispersion",
          function(object){
            cat('Query:', object@query, '; Dim:', object@dim, '\n\n')
          })

