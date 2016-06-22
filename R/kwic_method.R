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
#' @param cpos logical
#' @param pAttribute typically 'word' or 'lemma'
#' @param sAttribute if provided, the s-attribute will be used to check the boundaries of the text
#' @param neighbor only show kwic if a certain word is present
#' @param verbose logical, whether to be talkative
#' @param ... further parameters to be passed
#' @rdname kwic
#' @docType methods
#' @seealso To read the whole text, see the \code{\link{read}}-method.
#' @examples
#' \dontrun{
#' if (require(polmineR.sampleCorpus) && require(rcqp)){
#'   use("polmineR.sampleCorpus")
#'   bt <- partition("PLPRBTTXT", def=list(text_date=".*"), regex=TRUE)
#'   kwic(bt, "Integration")
#'   kwic(
#'     bt, "Integration",
#'     left=20, right=20,
#'     meta=c("text_date", "text_name", "text_party")
#'   )
#'   kwic(
#'     bt, '"Integration" [] "(Menschen|Migrant.*|Personen)"',
#'     left=20, right=20,
#'     meta=c("text_date", "text_name", "text_party")
#'   ) 
#' }
#' }
#' @exportMethod kwic
setGeneric("kwic", function(.Object, ...){standardGeneric("kwic")})


#' @exportMethod kwic
#' @docType methods
#' @rdname kwic
setMethod("kwic", "context", function(.Object, meta=getOption("polmineR.meta"), cpos=FALSE, neighbor=NULL, verbose=FALSE){
  
  tab <- lapply(
    c("left", "node", "right"),
    function(what){
      tokens <- unlist(lapply(
        .Object@cpos, function(x) {
          getTokenStream(
            x[[what]],
            corpus=.Object@corpus,
            encoding=.Object@encoding,
            pAttribute=.Object@pAttribute,
            collapse=" ",
            beautify=TRUE
            )
        }))
    }
  )
  tab <- data.frame(tab, stringsAsFactors=FALSE)
  colnames(tab) <- c("left", "node", "right")
  
  if (!is.null(meta)){
    metainformation <- lapply(
      meta,
      function(metadat){
        # sAttr <- paste(.Object@corpus, ".", metadat, sep="")
        strucs <- CQI$cpos2struc(.Object@corpus, metadat, unlist(lapply(.Object@cpos, function(x)x$node[1])))
        as.utf8(CQI$struc2str(.Object@corpus, metadat, strucs))
      }
    )
    metainformation <- data.frame(metainformation, stringsAsFactors = FALSE)
    colnames(metainformation) <- meta
    tab <- data.frame(metainformation, tab)  
  }
  
  if (length(neighbor) > 0){
    tab <- tab[grep(neighbor, apply(tab, 1, function(x)paste(x[length(x)-2], x[length(x)]))),]
  } 
  
  conc <- new(
    'kwic',
    corpus=.Object@corpus, left=.Object@left, right=.Object@right,
    table=tab,
    metadata=ifelse(is.null(meta), as.character(c()), meta),
    encoding=.Object@encoding
    )
  if (cpos == TRUE) conc@cpos <- .Object@cpos
  if (!is.null(neighbor) || !length(neighbor) == 0) {conc@neighbor <- neighbor}
  conc
})


#' @rdname kwic
#' @exportMethod kwic
setMethod("kwic", "partition", function(
  .Object, query,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  meta = getOption("polmineR.meta"),
  pAttribute="word", sAttribute=NULL, cpos=TRUE,
  neighbor=NULL,
  verbose=TRUE
){
  ctxt <- context(
    .Object=.Object, query=query,
    pAttribute=pAttribute, sAttribute=sAttribute,
    left=left, right=right,
    method=NULL, count=FALSE, verbose=verbose
  )
  if (is.null(ctxt)){
    message("... no occurrence of query")
    return(NULL)
    }
  kwic(.Object=ctxt, meta=meta, neighbor=neighbor, cpos=cpos)
})

#' @rdname kwic
setMethod("kwic", "missing", function(.Object, ...){
  if (requireNamespace("shiny", quietly=TRUE)){
    shiny::runApp(system.file("shiny", "kwic", package="polmineR"), launch.browser=TRUE)  
  } else {
    message("package shiny not available")
  }
  
})

#' @rdname kwic
setMethod("kwic", "character", function(
  .Object, query,
  left=getOption("polmineR.left"),
  right=getOption("polmineR.right"),
  meta=getOption("polmineR.meta"),
  pAttribute="word", sAttribute=NULL, cpos=FALSE,
  neighbor=NULL,
  verbose=TRUE
){
  hits <- cpos(.Object, query=query, pAttribute=pAttribute, verbose=FALSE)
  if (is.null(hits)) {
    message("sorry, not hits")
    return(NULL)
  }
  cposMax <- CQI$attribute_size(.Object, pAttribute)
  cposList <- apply(
    hits, 1,
    function(row){
      left <- c((row[1] - left - 1):(row[1] - 1))
      right <- c((row[2] + 1):(row[2] + right + 1))
      list(
        left=left[left > 0],
        node=c(row[1]:row[2]),
        right=right[right <= cposMax]
        )
    }
    )
  ctxt <- new(
    "context",
    count=nrow(hits), stat=data.table(),
    corpus=.Object,
    left=left, right=right, 
    cpos=cposList,
    pAttribute=pAttribute,
    encoding=parseRegistry(.Object)[["charset"]]
    )
  if (!is.null(sAttribute)) ctxt@sAttribute <- sAttribute
  kwic(.Object=ctxt, meta=meta, neighbor=neighbor, cpos=cpos)
})

