# this file contains the kwic method and helper functions for these methods

#' @include partition-class.R context-class.R
NULL 

setGeneric("kwic", function(object, ...){standardGeneric("kwic")})




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
  left <- unlist(lapply(ctxt@cpos, function(x) {paste(cqi_cpos2str(paste(ctxt@corpus,'.', ctxt@pAttribute, sep=""), x$left), collapse=" ")}))
  node <- unlist(lapply(ctxt@cpos, function(x) {paste(cqi_cpos2str(paste(ctxt@corpus,'.', ctxt@pAttribute, sep=""), x$node), collapse=" ")}))
  right <- unlist(lapply(ctxt@cpos, function(x) {paste(cqi_cpos2str(paste(ctxt@corpus,'.', ctxt@pAttribute, sep=""), x$right), collapse=" ")}))
  Encoding(left) <- ctxt@encoding
  Encoding(node) <- ctxt@encoding
  Encoding(right) <- ctxt@encoding  
  m <- cbind(m, left=left, node=node, right=right)
  if (length(collocate) > 0) m <- m[grep(collocate, apply(m, 1, function(x)paste(x[length(x)-2], x[length(x)]))),]
  m <- m[2:ncol(m)]
  colnames(m) <- c(metadata, c('leftContext', 'node', 'rightContext'))
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

#' @exportMethod kwic
#' @docType methods
setMethod("kwic", "context", function(object, metadata=NULL, collocate=c()){
  .kwic(ctxt=object, metadata=metadata, collocate=collocate)
})



#' KWIC output
#' 
#' Prepare and show 'keyword in context' (KWIC). The same result can be achieved by 
#' applying the kwich method on either a partition or a context object.
#' 
#' @param object a partition object
#' @param query what to look up
#' @param leftContext to the left
#' @param rightContext to the right
#' @param meta metainformation to display
#' @param pAttribute typically 'word' or 'lemma'
#' @param collocate only show kwic if a certain word is present
#' @aliases kwic,partition-method show,kwic-method kwic,context-method kwic
#' @docType methods
#' @examples
#' bt <- partition("PLPRTXT", def=list(text_date=".*"), method="grep")
#' foo <- kwic(bt, "Integration")
#' foo <- kwic(bt, "Integration", leftContext=20, rightContext=20, meta=c("text_date", "text_name", "text_party")) 
#' @exportMethod kwic
setMethod("kwic", "partition", function(
  object, query,
  leftContext=0,
  rightContext=0,
  meta=NULL,
  pAttribute="word",
  collocate=c()
){
  ctxt <- context(
    object=object, query=query, pAttribute=pAttribute,
    leftContext=leftContext, rightContext=rightContext,
    statisticalTest=NULL
  )
  .kwic(ctxt=ctxt, metadata=meta, collocate=collocate)
})

