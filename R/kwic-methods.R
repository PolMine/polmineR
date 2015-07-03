#' @include kwic-class.R kwic-method.R
NULL

#' @docType methods
setMethod('show', 'kwic', function(object){
  .showKwicLine <- function(object, i){
    metaoutput <- paste(as.vector(unname(unlist(object@table[i,c(1:length(object@metadata))]))), collapse=" | ")
    Encoding(metaoutput) <- object@encoding
    cat('[',metaoutput, '] ', sep='')
    cat(paste(as.vector(unname(unlist(object@table[i,c((ncol(object@table)-2):ncol(object@table))]))), collapse=" * "), "\n\n")
  }
  sessionKwicNo <- slot(get("session", '.GlobalEnv'), 'kwicNo')
  if (sessionKwicNo == 0 ) {
    for (i in 1:nrow(object@table)) .showKwicLine(object, i)
  } else if (sessionKwicNo > 0) {
    if (nrow(object@table) <= sessionKwicNo) {
      for (i in 1:nrow(object@table)) .showKwicLine(object, i)
    } else {
      chunks <- trunc(nrow(object@table)/sessionKwicNo)
      for ( i in c(0:(chunks-1))) {
        lines <- i*sessionKwicNo+c(1:sessionKwicNo)
        cat ('---------- KWIC output', min(lines), 'to', max(lines), 'of', nrow(object@table),'----------\n\n')
        for (j in lines) .showKwicLine(object, j)
        cat("(press 'q' to quit or ENTER to continue)\n")
        loopControl <- readline()
        if (loopControl == "q") break
      }
      if ((chunks*sessionKwicNo < nrow(object@table)) && (loopControl != "q")){
        cat ('---------- KWIC output', chunks*sessionKwicNo, 'to', nrow(object@table), 'of', nrow(object@table),'----------\n\n')
        lines <- c((chunks*sessionKwicNo):nrow(object@table))
        for (j in lines) .showKwicLine(object, j)
      }
    }
  }    
})

#' @docType methods
#' @noRd
setMethod('[', 'kwic',
          function(x,i) {
            x@table <- x@table[i,]
            x
          }        
)

#' @rdname kwic-class
setMethod("as.data.frame", "kwic", function(x){
  metaColumnsNo <- length(colnames(x@table)) - 3
  metadata <- apply(x@table, 1, function(row) paste(row[c(1:metaColumnsNo)], collapse="<br/>"))
  data.frame(
    meta=metadata,
    leftContext=x@table$leftContext,
    node=x@table$node,
    rightContext=x@table$rightContext
  )
})

