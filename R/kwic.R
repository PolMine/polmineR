#' KWIC output
#' 
#' Based on a context object, you get concordances, i.e. the context of a 
#' keyword
#' 
#' This functiongives you quite some flexibility to adjust the output to your needs.
#' Use drillingControls to adjust output.
#' 
#' @param ctxt a context object
#' @param metadata character vector with the metadata included in output
#' @param collocate limit output to a concordances containing a specific 
#'   collocate
#' @return a kwic object
#' @author Andreas Blaette
#' @noRd
.kwic <- function(ctxt, metadata=NULL, collocate=c()){
  if(is.null(metadata)) metadata <- get("drillingControls", '.GlobalEnv')[['kwicMetadata']]
  m <- data.frame(dummy=rep(0, length(ctxt@cpos)))
  if (all(is.element(metadata, cqi_attributes(ctxt@corpus, "s")))!=TRUE) {
    warning("check drillingControls$kwicMetadata: Not all sAttributes supplied are available in corpus")
  }
  for (meta in metadata){
    sattr <- paste(ctxt@corpus, ".", meta, sep="")
    strucs <- cqi_cpos2struc(sattr, unlist(lapply(ctxt@cpos, function(x)x$node[1])))
    m <- cbind(m, cqi_struc2str(sattr, strucs))
  }
  left <- unlist(lapply(ctxt@cpos, function(x) {paste(cqi_cpos2str(paste(ctxt@corpus,'.', ctxt@pattribute, sep=""), x$left), collapse=" ")}))
  node <- unlist(lapply(ctxt@cpos, function(x) {paste(cqi_cpos2str(paste(ctxt@corpus,'.', ctxt@pattribute, sep=""), x$node), collapse=" ")}))
  right <- unlist(lapply(ctxt@cpos, function(x) {paste(cqi_cpos2str(paste(ctxt@corpus,'.', ctxt@pattribute, sep=""), x$right), collapse=" ")}))
  Encoding(left) <- ctxt@encoding
  Encoding(node) <- ctxt@encoding
  Encoding(right) <- ctxt@encoding  
  m <- cbind(m, left=left, node=node, right=right)
  if (length(collocate) > 0) m <- m[grep(collocate, apply(m, 1, function(x)paste(x[length(x)-2], x[length(x)]))),]
  m <- m[2:ncol(m)]
  colnames(m) <- c(metadata, c('left.context', 'node', 'right.context'))
  conc <- new('kwic')
  if (!is.null(collocate)) {conc@collocate <- collocate}
  conc@table <- m
  conc@metadata <- metadata
  conc@encoding <- ctxt@encoding
  conc
}




.showKwicLine <- function(object, i){
  drillingControls <- get("drillingControls", '.GlobalEnv')
  metaoutput <- paste(as.vector(unname(unlist(object@table[i,c(1:length(object@metadata))]))), collapse=" | ")
  Encoding(metaoutput) <- object@encoding
  if (drillingControls$xtermStyle==FALSE){
    cat('[',metaoutput, '] ', sep='')
  } else {
    cat(style(paste('[',metaoutput, ']',sep=''),fg=drillingControls$xtermFgMeta,bg=drillingControls$xtermBgMeta), ' ', sep='')
  }
  if (drillingControls$xtermStyle==FALSE){
    cat(paste(as.vector(unname(unlist(object@table[i,c((ncol(object@table)-2):ncol(object@table))]))), collapse=" * "), "\n\n")
  } else {
    if (length(object@collocate)==0){object@collocate="FOO"}
    foo <- sapply(unlist(strsplit(as.vector(unname(unlist(object@table[i,ncol(object@table)-2]))), ' ')),
                  function(x){
                    if (x==object@collocate){
                      cat(style(x, bg=drillingControls$xtermBgCollocate, fg=drillingControls$xtermFgCollocate), ' ')
                    } else {cat(x, ' ', sep='')}
                  }
    )
    cat(' ', style(object@table[i,ncol(object@table)-1], bg=drillingControls$xtermBgNode, fg=drillingControls$xtermFgNode), ' ', sep='')
    foo <- sapply(unlist(strsplit(as.vector(unname(unlist(object@table[i,ncol(object@table)]))), ' ')),
                  function(x){
                    if (x==object@collocate){
                      cat(style(x, bg=drillingControls$xtermBgCollocate, fg=drillingControls$xtermFgCollocate), ' ')
                    } else {cat(x, ' ', sep='')}
                  }
    )                                                     
    cat("\n\n")
  }
}

#' @importFrom xtermStyle style
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


setMethod('[', 'kwic',
          function(x,i) {
            x@table <- x@table[i,]
            x
          }        
)
