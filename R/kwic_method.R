#' @include partition_class.R context_class.R
NULL 

#' KWIC output / concordances
#' 
#' Prepare and show concordances / keyword-in-context (kwic). The same result can be achieved by 
#' applying the kwich method on either a partition or a context object.
#' 
#' If you enter \code{"kwic()"} on the console, a shiny application will be launched. The app
#' will offer partition objects present in the global environment.
#' 
#' @param .Object a \code{partition} or \code{context} object, if \code{missing}, a shiny application will be launched
#' @param query a query, CQP-syntax can be used, then use 
#' @param left to the left
#' @param right to the right
#' @param meta metainformation to display
#' @param pAttribute typically 'word' or 'lemma'
#' @param sAttribute if provided, the s-attribute will be used to check the boundaries of the text
#' @param neighbor only show kwic if a certain word is present
#' @param verbose logical, whether to be talkative
#' @param ... further parameters to be passed
#' @rdname kwic
#' @docType methods
#' @seealso To read the whole text, see the \code{\link{read}}-method.
#' @examples
#' use("polmineR.sampleCorpus")
#' bt <- partition("PLPRBTTXT", def=list(text_date=".*"), regex=TRUE)
#' kwic(bt, "Integration")
#' kwic(bt, "Integration", left=20, right=20, meta=c("text_date", "text_name", "text_party"))
#' kwic(bt, '"Integration" [] "(Menschen|Migrant.*|Personen)"', left=20, right=20, meta=c("text_date", "text_name", "text_party")) 
#' @exportMethod kwic
setGeneric("kwic", function(.Object, ...){standardGeneric("kwic")})


#' @exportMethod kwic
#' @docType methods
#' @rdname kwic
setMethod("kwic", "context", function(.Object, meta=NULL, neighbor=NULL){
  if(is.null(meta)){
    parsedRegistry <- parseRegistry(.Object@corpus)
    if ("meta" %in% names(parsedRegistry)){
      meta <- parsedRegistry[["meta"]]
    } else {
      meta <- slot(get("session", '.GlobalEnv'), 'kwicMetadata')
      if (all(meta %in% sAttributes(.Object@corpus)) == FALSE){
        stop("meta found in session settings does not work") 
      }
    }
  }
    
  
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
  colnames(conc) <- c("left", "node", "right")
  tab <- data.frame(metainformation, conc)
  if (length(neighbor) > 0){
    tab <- tab[grep(neighbor, apply(tab, 1, function(x)paste(x[length(x)-2], x[length(x)]))),]
  } 
  conc <- new(
    'kwic',
    left=.Object@left, right=.Object@right,
    table=tab, metadata=meta, encoding=.Object@encoding
    )
  if (!is.null(neighbor)) {conc@neighbor <- neighbor}
  conc
})


#' @rdname kwic
#' @exportMethod kwic
setMethod("kwic", "partition", function(
  .Object, query,
  left=NULL, right=NULL,
  meta=NULL, pAttribute="word", sAttribute=NULL,
  neighbor=NULL,
  verbose=TRUE
){
  ctxt <- context(
    .Object=.Object, query=query,
    pAttribute=pAttribute, sAttribute=sAttribute,
    left=left, right=right,
    method=NULL, verbose=verbose
  )
  if (is.null(ctxt)){
    message("... no occurrence of query")
    return(NULL)
    }
  kwic(ctxt, meta=meta, neighbor=neighbor)
})

#' @rdname kwic
setMethod("kwic", "missing", function(.Object, ...){
  if (requireNamespace("shiny", quietly=TRUE)){
    shiny::runApp(system.file("shiny", "kwic", package="polmineR"), launch.browser=TRUE)  
  } else {
    message("package shiny not available")
  }
  
})
