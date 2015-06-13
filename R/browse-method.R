#' @include polmineR-package.R kwic-class.R context-class.R
NULL


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
setMethod("browse", "context", function(object){
  if (require("DataTablesR", quietly=TRUE)){
    tab <- data.frame(
      token=rownames(object@stat),
      object@stat
    )
    tab[, "expCoi"] <- round(tab[, "expCoi"], 2)
    tab[, "expCorpus"] <- round(tab[, "expCorpus"], 2)
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

