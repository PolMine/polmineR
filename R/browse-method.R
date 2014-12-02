#' @include polmineR-package.R
NULL

#' @import DataTablesR
NULL

setMethod("as.DataTables", "textstat", function(object){
  as.DataTables(
    data.frame(
      token=colnames(object@stat),
      object@stat
      )
    )
})

setMethod("browse", "textstat", function(object){
  htmlDoc <- as.DataTables(object)
  browse(htmlDoc)
})

setMethod("as.DataTables", "context", function(object){
  tab <- data.frame(
    token=colnames(object@stat),
    object@stat
  )
  tab[, "expCoi"] <- round(tab[, "expCoi"], 2)
  tab[, "expCorpus"] <- round(tab[, "expCorpus"], 2)
  for (what in object@statisticalTest){
    tab[, what] <- round(tab[, what], 2)
  }
  htmlDoc <- as.DataTables(tab)
  return(htmlDoc)
})

setMethod("browse", "context", function(object){
  htmlDoc <- as.DataTables(object)
  browse(htmlDoc)
})

