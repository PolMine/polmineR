#' @include polmineR-package.R kwic-class.R context-class.R
NULL



setMethod("browse", "textstat", function(object){
  if (require("DataTablesR", quietly=TRUE)){
    htmlDoc <- DataTablesR::as.DataTables(
      data.frame(
        token=rownames(object@stat),
        object@stat
      )
    )
    retval <- browse(htmlDoc)
  } else {
    warning("the 'DataTablesR'-package needs to be installed")
    stop()
  }
  retval
})

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
    htmlDoc <- DataTablesR::as.DataTables(tab)
    retval <- browse(htmlDoc)
  } else {
    warning("package 'DataTablesR' needs to be installed but is not available")
    stop()
  }
  retval
})


#' @rdname partition
setMethod("browse", "partition", function(object){
  htmlDoc <- html(object)
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

