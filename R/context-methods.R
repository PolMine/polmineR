#' @include context-class.R
NULL


#' @docType methods
setMethod('summary', 'context',
  function(object) {
  .statisticalSummary(object)
  
}
)

#' @docType methods
setMethod('head', 'context', function(x, n=10) {
  head(x@stat, n=n)
})

#' @docType methods
setMethod('tail', 'context', function(x, n=10){
  tail(x@stat, n=n)
})

#' @docType methods
setMethod('show', 'context',
          function(object) {
            cat("\n** Context object - general information: **\n")
            cat(sprintf("%-20s", "CWB-Korpus:"), object@corpus, "\n")
            cat(sprintf("%-20s", "Partition:"), object@partition, "\n")
            cat(sprintf("%-20s", "Node:"), object@query, "\n")
            cat(sprintf("%-20s", "P-Attribute:"), object@pAttribute, "\n")
            cat(sprintf("%-20s", "Node count:"), object@frequency, "\n")
            cat(sprintf("%-20s", "Stat table length:"), nrow(object@stat), "\n\n")
})

#' @docType methods  
setMethod('[', 'context',
          function(x,i) {
            settingKwicMetadata <- slot(get("session", '.GlobalEnv'), "kwicMetadata")
            conc <- kwic(x, metadata=settingKwicMetadata)
            conc@table <- conc@table[i,]
            show(conc)
          }        
)

#' @docType methods
setMethod('[[', 'context',
  function(x,i) {
    sessionKwicMetadata <- slot(get("session", '.GlobalEnv'), "kwicMetadata")
    conc <- kwic(x, metadata=sessionKwicMetadata, collocate=i)
    foo <- show(conc)
  }        
)

