#' @include polmineR.R kwic.R context.R S4classes.R
NULL

#' Display in browser
#' 
#' @param object what is to be displayed
#' @param meta metainformation to be displayed
#' @param colnames colnames to be used for data.frame
#' @param ... further parameters
#' @rdname browse
#' @name browse
#' @exportMethod browse
setGeneric("browse", function(object, ...) standardGeneric("browse"))


#' @rdname browse
setMethod("browse", "textstat", function(object){
  if (require("DataTablesR", quietly=TRUE)){
    htmlDir <- DataTablesR::as.DataTables(
      data.frame(
        token=rownames(object@stat),
        object@stat
      )
    )
    retval <- show(htmlDir)
  } else {
    warning("the 'DataTablesR'-package needs to be installed")
    stop()
  }
  retval
})

#' @rdname browse
setMethod("browse", "cooccurrences", function(object){
  if (require("DataTablesR", quietly = TRUE)){
    tab <- data.frame(
      token=rownames(object@stat),
      object@stat
    )
    tab[, "exp_window"] <- round(tab[, "exp_window"], 2)
    tab[, "exp_partition"] <- round(tab[, "exp_partition"], 2)
    for (what in object@statisticalTest){
      tab[, what] <- round(tab[, what], 2)
    }
    htmlDir <- DataTablesR::as.DataTables(tab)
    retval <- show(htmlDir)
  } else {
    warning("package 'DataTablesR' needs to be installed but is not available")
    stop()
  }
  retval
})

#' @rdname browse
setMethod("browse", "partition", function(object, meta=NULL){
  htmlDoc <- html(object, meta=meta)
  tmpFile <- tempfile()
  cat(htmlDoc, file=tmpFile)
  browseURL(tmpFile)
})

#' @rdname browse
setMethod("browse", "html", function(object){
  tmpFile <- tempfile(fileext=".html")
  cat(object, file=tmpFile)
  browseURL(tmpFile)
  return(tmpFile)
})



#' @rdname browse
setMethod("browse", "kwic", function(object, colnames=NULL){
  if (requireNamespace("DataTablesR", quietly=TRUE)){
    tab <- as.data.frame(object)
    if (!is.null(colnames)){
      colnames(tab) <- colnames
    }
    htmlDir <- DataTablesR::as.DataTables(tab, align=c("l", "r", "c", "l"))    
  } else {
    warning("package 'DataTablesR' needs to be installed but is not available")
    stop()
  }
  retval <- show(htmlDir)
  retval
})


 
#' @rdname browse
#' @exportMethod browse
setMethod("browse", "press_partition", function(object, meta = c("text_newspaper", "text_date")){
  callNextMethod(object = object, meta = meta)
})

