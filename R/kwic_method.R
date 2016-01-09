#' @include partition_class.R context_class.R
NULL 

#' KWIC output
#' 
#' Prepare and show 'keyword in context' (KWIC). The same result can be achieved by 
#' applying the kwich method on either a partition or a context object.
#' 
#' @param .Object a partition object
#' @param query what to look up
#' @param leftContext to the left
#' @param rightContext to the right
#' @param meta metainformation to display
#' @param pAttribute typically 'word' or 'lemma'
#' @param neighbor only show kwic if a certain word is present
#' @param verbose whether to be talkative
#' @param ... further parameters to be passed
#' @aliases kwic,partition-method show,kwic-method kwic,context-method kwic
#' @rdname kwic
#' @docType methods
#' @examples
#' bt <- partition("PLPRTXT", def=list(text_date=".*"), regex=TRUE)
#' foo <- kwic(bt, "Integration")
#' foo <- kwic(bt, "Integration", leftContext=20, rightContext=20, meta=c("text_date", "text_name", "text_party")) 
#' @exportMethod kwic
setGeneric("kwic", function(.Object, ...){standardGeneric("kwic")})


#' @exportMethod kwic
#' @docType methods
#' @rdname kwic
setMethod("kwic", "context", function(.Object, meta=NULL, neighbor=NULL){
  if(is.null(meta)) meta <- slot(get("session", '.GlobalEnv'), 'kwicMetadata')
  stopifnot(all(meta %in% sAttributes(.Object@corpus)))
  metainformation <- lapply(
    meta,
    function(metadat){
      sAttr <- paste(.Object@corpus, ".", metadat, sep="")
      strucs <- cqi_cpos2struc(sAttr, unlist(lapply(.Object@cpos, function(x)x$node[1])))
      as.utf8(cqi_struc2str(sAttr, strucs))
    }
    )
  metainformation <- data.frame(metainformation, stringsAsFactors = FALSE)
  colnames(metainformation) <- meta
  conc <- lapply(
    c("left", "node", "right"),
    function(what){
      tokens <- unlist(lapply(.Object@cpos, function(x) {paste(cqi_cpos2str(paste(.Object@corpus,'.', .Object@pAttribute, sep=""), x[[what]]), collapse=" ")}))
      Encoding(tokens) <- .Object@encoding
      as.utf8(tokens)
    }
  )
  conc <- data.frame(conc, stringsAsFactors=FALSE)
  colnames(conc) <- c("leftContext", "node", "rightContext")
  tab <- data.frame(metainformation, conc)
  if (length(neighbor) > 0){
    tab <- tab[grep(neighbor, apply(tab, 1, function(x)paste(x[length(x)-2], x[length(x)]))),]
  } 
  conc <- new(
    'kwic',
    leftContext=.Object@leftContext, rightContext=.Object@rightContext,
    table=tab, metadata=meta, encoding=.Object@encoding
    )
  if (!is.null(neighbor)) {conc@neighbor <- neighbor}
  conc
})


#' @rdname kwic
#' @exportMethod kwic
setMethod("kwic", "partition", function(
  .Object, query,
  leftContext=NULL, rightContext=NULL,
  meta=NULL, pAttribute="word", neighbor=NULL,
  verbose=TRUE
){
  ctxt <- context(
    .Object=.Object, query=query, pAttribute=pAttribute,
    leftContext=leftContext, rightContext=rightContext,
    statisticalTest=NULL, verbose=verbose
  )
  if (is.null(ctxt)){
    message("... no occurrence of query")
    return(NULL)
    }
  kwic(ctxt, meta=meta, neighbor=neighbor)
})


#' @rdname kwic
setMethod("kwic", "plprPartition", function(
  object, query, leftContext=5, rightContext=5,
  meta=NULL, pAttribute="word", neighbor=c(), verbose=TRUE
){
  Partition <- new("partition")
  for (x in slotNames(object)) slot(Partition, x) <- slot(object, x)
  kwicObject <- kwic(
    Partition, query=query, leftContext=leftContext,
    rightContext=rightContext, meta=meta, pAttribute=pAttribute,
    neighbor=neighbor, verbose=verbose
  )
  if (is.null(kwicObject)) {
    message("... no hits for query")
    return()
  }
  plprKwicObject <- as(kwicObject, "plprKwic")
  plprKwicObject@sAttributes <- object@sAttributes
  plprKwicObject@corpus <- object@corpus
  plprKwicObject
})

