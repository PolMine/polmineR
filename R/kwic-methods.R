#' @include kwic-class.R kwic-method.R
NULL

#' @importFrom xtermStyle style
#' @docType methods
setMethod('show', 'kwic', function(object){
  drillingControls <- get("drillingControls", '.GlobalEnv')
  if (drillingControls$kwicNo == 0 ) {
    for (i in 1:nrow(object@table)) .showKwicLine(object, i)
  } else if (drillingControls$kwicNo > 0) {
    if (nrow(object@table) <= drillingControls$kwicNo) {
      for (i in 1:nrow(object@table)) .showKwicLine(object, i)
    } else {
      chunks <- trunc(nrow(object@table)/drillingControls$kwicNo)
      for ( i in c(0:(chunks-1))) {
        lines <- i*drillingControls$kwicNo+c(1:drillingControls$kwicNo)
        cat ('---------- KWIC output', min(lines), 'to', max(lines), 'of', nrow(object@table),'----------\n\n')
        for (j in lines) .showKwicLine(object, j)
        cat("(press 'q' to quit or ENTER to continue)\n")
        loopControl <- readline()
        if (loopControl == "q") break
      }
      if ((chunks*drillingControls$kwicNo < nrow(object@table)) && (loopControl != "q")){
        cat ('---------- KWIC output', chunks*drillingControls$kwicNo, 'to', nrow(object@table), 'of', nrow(object@table),'----------\n\n')
        lines <- c((chunks*drillingControls$kwicNo):nrow(object@table))
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

