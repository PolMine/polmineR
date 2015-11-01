#' @include context_class.R
NULL


#' @docType methods
setMethod('summary', 'context',
  function(object) {
    cat("\n** Context object - general information: **\n")
    cat(sprintf("%-20s", "CWB-Korpus:"), object@corpus, "\n")
    cat(sprintf("%-20s", "Partition:"), object@partition, "\n")
    cat(sprintf("%-20s", "Node:"), object@query, "\n")
    cat(sprintf("%-20s", "P-Attribute:"), object@pAttribute, "\n")
    cat(sprintf("%-20s", "Node count:"), object@frequency, "\n")
    cat(sprintf("%-20s", "Stat table length:"), nrow(object@stat), "\n\n")
    return(.statisticalSummary(object))
  
})

#' @docType methods
setMethod('head', 'context', function(x, n=10) {
  head(x@stat, n=n)
})

#' @rdname context-class
#' @docType methods
setMethod('tail', 'context', function(x, n=10){
  tail(x@stat, n=n)
})

#' @docType methods
setMethod('show', 'context', function(object) {
  roundedTextstatObject <- as.data.frame(round(object))
  if (Sys.getenv("RSTUDIO") == "1"){
    View(roundedTextstatObject)
  } else {
    if (slot(get("session", '.GlobalEnv'), "browse") == TRUE){
      browse(roundedTextstatObject)  
    } else {
      return(roundedTextstatObject) 
    }
  }
})

#' @docType methods  
setMethod('[', 'context',
          function(x,i) {
            settingKwicMetadata <- slot(get("session", '.GlobalEnv'), "kwicMetadata")
            conc <- kwic(x, meta=settingKwicMetadata)
            conc@table <- conc@table[i,]
            show(conc)
          }        
)

#' @docType methods
#' @rdname context-class
setMethod('[[', 'context',
  function(x,i) {
    sessionKwicMetadata <- slot(get("session", '.GlobalEnv'), "kwicMetadata")
    conc <- kwic(x, metadata=sessionKwicMetadata, cooccurrence=i)
    foo <- show(conc)
  }        
)

