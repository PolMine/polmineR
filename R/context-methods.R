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
  x@stat[1:n,c(1,3,4,7)]
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
            drillingControls <- get("drillingControls", '.GlobalEnv')
            conc <- kwic(x, metadata=drillingControls$kwicMetadata)
            conc@table <- conc@table[i,]
            show(conc)
          }        
)

#' @docType methods
setMethod('[[', 'context',
  function(x,i) {
    drillingControls <- get("drillingControls", '.GlobalEnv')
    conc <- kwic(x, metadata=drillingControls$kwicMetadata, collocate=i)
    foo <- show(conc)
  }        
)

