# this file contains the kwic method and helper functions for these methods
#' @include partition_class.R context_class.R
NULL 

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
#' @param verbose whether to be talkative
#' @param ... further parameters to be passed
#' @aliases kwic,partition-method show,kwic-method kwic,context-method kwic
#' @rdname kwic
#' @docType methods
#' @examples
#' bt <- partition("PLPRTXT", def=list(text_date=".*"), method="grep")
#' foo <- kwic(bt, "Integration")
#' foo <- kwic(bt, "Integration", leftContext=20, rightContext=20, meta=c("text_date", "text_name", "text_party")) 
#' @exportMethod kwic
setGeneric("kwic", function(object, ...){standardGeneric("kwic")})


#' @exportMethod kwic
#' @docType methods
#' @rdname kwic
setMethod("kwic", "context", function(object, meta=NULL, collocate=c()){
  ctxt <- object
  if(is.null(meta)) meta <- slot(get("session", '.GlobalEnv'), 'kwicMetadata')
  m <- data.frame(dummy=rep(0, length(ctxt@cpos)))
  if (all(is.element(meta, cqi_attributes(ctxt@corpus, "s")))!=TRUE) {
    warning("check session settings: Not all sAttributes supplied are available in corpus")
  }
  for (metadat in meta){
    sattr <- paste(ctxt@corpus, ".", metadat, sep="")
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
  colnames(m) <- c(meta, c('leftContext', 'node', 'rightContext'))
  conc <- new(
    'kwic', leftContext=object@leftContext, rightContext=object@rightContext
    )
  if (!is.null(collocate)) {conc@collocate <- collocate}
  conc@table <- m
  conc@metadata <- meta
  conc@encoding <- ctxt@encoding
  conc
})


#' @rdname kwic
setMethod("kwic", "partition", function(
  object, query,
  leftContext=NULL, rightContext=NULL,
  meta=NULL, pAttribute="word", collocate=c(),
  verbose=TRUE
){
  ctxt <- context(
    object=object, query=query, pAttribute=pAttribute,
    leftContext=leftContext, rightContext=rightContext,
    statisticalTest=NULL, verbose=verbose
  )
  kwic(ctxt, meta=meta, collocate=collocate)
})

