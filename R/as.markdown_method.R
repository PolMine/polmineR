#' Generate markdown from a partition.
#' 
#' The method is the worker behind the html-method.
#' 
#' @param .Object object to be converted
#' @param corpus name of CWB corpus
#' @param meta metainformation i.e. s-attributes) to be displayed
#' @param cpos logical, whether to add cpos as ids in span elements
#' @param interjections logical, whether to format interjections
#' @param cutoff maximum number of tokens to reconstruct
#' @param ... further arguments
#' @rdname as.markdown
#' @exportMethod as.markdown
setGeneric("as.markdown", function(.Object, ...) standardGeneric("as.markdown"))

# helper function to wrap span with id around tokens
.wrapTokens <- function(tokens){
  sapply(
    c(1:length(tokens)),
    function(i){
      paste('<span id="', names(tokens)[i], '">', tokens[i], "</span>", sep="")
    }
  )
}

#' @rdname as.markdown
setMethod("as.markdown", "numeric", function(.Object, corpus, meta = NULL, cpos = FALSE, cutoff = NULL, ...){
  corpusEncoding <- parseRegistry(corpus)[["encoding"]]
  
  # generate metainformation
  documentStruc <- CQI$cpos2struc(corpus, getTemplate(corpus)[["document"]][["sAttribute"]], .Object[1])
  metaInformation <- meta(corpus, sAttributes = meta, struc = documentStruc)
  metaInformation <- paste(metaInformation, collapse=", ") # string will be converted to UTF-8
  metaInformation <- paste(
    getTemplate(corpus)[["document"]][["format"]][1],
    metaInformation,
    getTemplate(corpus)[["document"]][["format"]][2],
    sep = ""
    )
  
  cposSeries <- c(.Object[1]:.Object[2])
  pStrucs <- CQI$cpos2struc(corpus, getTemplate(corpus)[["paragraphs"]][["sAttribute"]], cposSeries)
  chunks <- split(cposSeries, pStrucs)
  pTypes <- CQI$struc2str(corpus, getTemplate(corpus)[["paragraphs"]][["sAttribute"]], as.numeric(names(chunks)))
  bodyList <- Map(
    function(pType, chunk){
      tokens <- getTokenStream(
        chunk, corpus = corpus, pAttribute = "word",
        encoding = corpusEncoding, cpos = cpos, cutoff = cutoff
      )
      if (cpos == TRUE) tokens <- .wrapTokens(tokens)
      paste(
        getTemplate(corpus)[["paragraphs"]][["format"]][[pType]][1],
        paste(tokens, collapse=" "),
        getTemplate(corpus)[["paragraphs"]][["format"]][[pType]][2],
        sep = ""
      )
    },
    pTypes, chunks
  )
  article <- c(metaInformation, unlist(bodyList))
  markdown <- paste(article, collapse="\n\n")
  markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
  markdown
})


#' @rdname partition-class
setMethod(
  "as.markdown", "partition",
  function(
    .Object,
    meta = getOption("polmineR.meta"),
    cpos = TRUE, cutoff = NULL,
    ...
  ){
    # ensure that template requested is available
    stopifnot(.Object@corpus %in% getTemplate())
    templateUsed <- getTemplate(.Object@corpus)
    if (is.null(templateUsed[["paragraphs"]])){
      tokens <- getTokenStream(.Object, pAttribute = "word", cpos = cpos, cutoff = cutoff, ...)
      if (cpos) tokens <- .wrapTokens(tokens)
      tokens <- paste(tokens, collapse = " ")
      rawTxt <- paste(tokens, sep = "\n")
      txt <- gsub("(.)\\s([,.:!?])", "\\1\\2", rawTxt)
    } else {
      articles <- apply(
        .Object@cpos, 1,
        function(row) as.markdown(row, corpus = .Object@corpus, meta = meta, cutoff = cutoff, ...)
        )
      txt <- paste(c("\n", unlist(articles)), collapse='\n* * *\n')
    }
    txt
  })

#' @rdname as.markdown
setMethod("as.markdown", "plprPartition", function(.Object, meta = NULL, cpos = FALSE, interjections = TRUE, cutoff = NULL, ...){
  # in the function call, meta is actually not needed, required by the calling function
  templateUsed <- getTemplate(.Object@corpus)
  if (is.null(meta)) meta <- templateUsed[["metadata"]]
  if (interjections == TRUE){
    maxNoStrucs <- .Object@strucs[length(.Object@strucs)] - .Object@strucs[1] + 1
    if (maxNoStrucs != length(.Object@strucs)){
      .Object@strucs <- c(.Object@strucs[1]:.Object@strucs[length(.Object@strucs)])
      .Object@cpos <- do.call(rbind, lapply(
        .Object@strucs,
        function(i) CQI$struc2cpos(.Object@corpus, .Object@sAttributeStrucs, i)
      ))
    }
    # this is potential double work, enrich is also performed in the html-method
    .Object <- enrich(.Object, meta = meta, verbose=FALSE)
  }
  if (length(.Object@strucs) > 1){
    gapSize <- .Object@strucs[2:length(.Object@strucs)] - .Object@strucs[1:(length(.Object@strucs)-1)]
    gap <- c(0, vapply(gapSize, FUN.VALUE=1, function(x) ifelse(x >1, 1, 0) ))
    metaNo <- ncol(.Object@metadata)
    metaComp <- data.frame(
      .Object@metadata[2:nrow(.Object@metadata),],
      .Object@metadata[1:(nrow(.Object@metadata)-1),]
      )
    metaChange <- !apply(
      metaComp, 1,
      function(x) all(x[1:metaNo] == x[(metaNo+1):length(x)])
    )
    metaChange <- c(TRUE, metaChange)
    metadata <- apply(.Object@metadata, 2, function(x) as.vector(x))
  } else {
    gap <- 0
    metaChange <- TRUE
    metadata <- matrix(apply(.Object@metadata, 2, function(x) as.vector(x)), nrow = 1)
  }
  type <- CQI$struc2str(.Object@corpus, templateUsed[["speech"]][["sAttribute"]], .Object@strucs)
  
  if (is.numeric(cutoff)){
    beyondCutoff <- which(cumsum(.Object@cpos[,2] - .Object@cpos[,1]) > cutoff)
    if (length(beyondCutoff) > 0){
      threshold <- min(beyondCutoff)
      if (threshold > 1){
        metadata <- metadata[1:threshold,]
        .Object@cpos <- .Object@cpos[1:threshold,]
      }
    }
  }
  
  markdown <- sapply(c(1:nrow(metadata)), function(i) {
    meta <- c("")
    if (metaChange[i] == TRUE) { 
      meta <- paste(metadata[i,], collapse=" | ", sep="")
      meta <- paste(
        templateUsed[["document"]][["format"]][1],
        meta,
        templateUsed[["document"]][["format"]][2],
        collapse = ""
        )
      meta <- adjustEncoding(meta, "latin1")
    }
    tokens <- getTokenStream(
      matrix(.Object@cpos[i,], nrow = 1),
      corpus = .Object@corpus, pAttribute = "word", encoding = .Object@encoding,
      cpos = cpos
    )
    if (cpos == TRUE) tokens <- .wrapTokens(tokens)
    plainText <- paste(tokens, collapse = " ")
    plainText <- paste(
      templateUsed[["speech"]][["format"]][[type[i]]][1],
      plainText,
      templateUsed[["speech"]][["format"]][[type[i]]][1],
      sep = ""
      )
    paste(meta, plainText)
  })
  markdown <- paste(markdown, collapse="\n\n")
  markdown <- adjustEncoding(markdown, "UTF8")
  markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
  markdown <- gsub("\n - ", "\n", markdown)
  markdown
})

#' 
#' #' @rdname as.markdown
#' setMethod("as.markdown", "pressPartition", function(.Object, meta=c("text_newspaper", "text_date"), cpos=FALSE){
#'   articles <- apply(.Object@cpos, 1, function(row) {
#'     cposSeries <- c(row[1]:row[2])
#'     textStruc <- CQI$cpos2struc(.Object@corpus, ".text", row[1])
#'     pStrucs <- CQI$cpos2struc(.Object@corpus, ".p_type", cposSeries)
#'     chunks <- split(cposSeries, pStrucs)
#'     pTypes <- CQI$struc2str(.Object@corpus, ".p_type", as.numeric(names(chunks)))
#'     article <- Map(
#'       function(pType, chunk){
#'         tokens <- getTokenStream(
#'           matrix(c(chunk[1], chunk[length(chunk)]), nrow=1),
#'           corpus=.Object@corpus, pAttribute="word", encoding=.Object@encoding,
#'           cpos=cpos
#'         )
#'         if (cpos == TRUE) tokens <- .wrapTokens(tokens)
#'         para <- paste(tokens, collapse=" ")
#'         # Encoding(para) <- .Object@encoding
#'         #para <- as.utf8(para)
#'         para
#'       },
#'       pTypes, chunks
#'     )
#'     metaInformation <- sapply(
#'       meta,
#'       function(x) {
#'         retval <- CQI$struc2str(.Object@corpus, x, textStruc)
#'         Encoding(retval) <- .Object@encoding
#'         retval <- as.utf8(retval)
#'         retval
#'       })
#'     metaInformation <- paste(metaInformation, collapse=", ") # string will be converted to UTF-8
#'     article <- c(
#'       meta=metaInformation,
#'       article)
#'     .formattingFactory <- list(
#'       meta = function(x) paste("### ", x, sep=""),
#'       title = function(x) paste("## ", x, sep=""),
#'       teaser = function(x) paste("_", x, "_\n", sep=""),
#'       body = function(x) paste(x, "\n", sep=""),
#'       highlight = function(x) paste("_", x, "_\n", sep=""),
#'       headline = function(x) paste("# ", x, sep="")
#'     )
#'     formattedArticle <- lapply(
#'       c(1:length(article)),
#'       function(x) .formattingFactory[[names(article)[x]]](article[[x]])
#'     )
#'     markdown <- paste(formattedArticle, collapse="\n\n")
#'     # markdown <- as.utf8(markdown)
#'     markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
#'     markdown
#'   })
#'   paste(c("\n", unlist(articles)), collapse='\n* * *\n')
#' })
#' 
#' 
