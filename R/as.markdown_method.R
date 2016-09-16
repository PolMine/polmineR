#' Turn partition into markdown
#' 
#' The method is the worker behind the html-method.
#' 
#' @param .Object object to be converted
#' @param meta metainformation i.e. s-attributes) to be displayed
#' @param cpos logical, whether to add cpos as ids in span elements
#' @param interjections logical, whether to format interjections
#' @param ... further arguments
#' @rdname as.markdown
#' @exportMethod as.markdown
setGeneric("as.markdown", function(.Object, ...) {standardGeneric("as.markdown")})

# helper function to wrap span with id around tokens
.wrapTokens <- function(tokens){
  sapply(
    c(1:length(tokens)),
    function(i){
      paste('<span id="', names(tokens)[i], '">', tokens[i], "</span>", sep="")
    }
  )
}

#' @rdname partition-class
setMethod("as.markdown", "partition", function(.Object, meta, cpos = TRUE){
  tokens <- getTokenStream(.Object, pAttribute = "word", cpos = cpos)
  if (cpos) tokens <- .wrapTokens(tokens)
  tokens <- paste(tokens, collapse=" ")
  rawTxt <- paste(tokens, sep="\n")
  gsub("(.)\\s([,.:!?])", "\\1\\2", rawTxt)
})

#' @rdname as.markdown
setMethod("as.markdown", "plprPartition", function(.Object, meta, cpos=FALSE, interjections=TRUE){
  # in the function call, meta is actually not needed, its required by the calling function
  if (length(meta) == 0) stop("meta needs to be provided, either as session setting, or directly")
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
    .Object <- enrich(.Object, meta=meta, verbose=FALSE)
  }
  sAttribute <- grep("_type", sAttributes(.Object), value=T)[1]
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
    metadata <- matrix(apply(.Object@metadata, 2, function(x) as.vector(x)), nrow=1)
  }
  type <- CQI$struc2str(.Object@corpus, sAttribute, .Object@strucs)
  markdown <- sapply(c(1:nrow(metadata)), function(i) {
    meta <- c("")
    if (metaChange[i] == TRUE) { 
      meta <- paste(metadata[i,], collapse=" | ", sep="")
      meta <- paste("\n###", meta, "\n", collapse="")
      meta <- adjustEncoding(meta, "latin1")
    }
    tokens <- getTokenStream(
      matrix(.Object@cpos[i,], nrow=1),
      corpus=.Object@corpus, pAttribute="word", encoding=.Object@encoding,
      cpos=cpos
    )
    if (cpos == TRUE) tokens <- .wrapTokens(tokens)
    plainText <- paste(tokens, collapse=" ")
    if (type[i] == "interjection") plainText <- paste("\n> ", plainText, "\n", sep="")
    paste(meta, plainText)
  })
  markdown <- paste(markdown, collapse="\n\n")
  markdown <- adjustEncoding(markdown, "UTF8")
  markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
  markdown <- gsub("\n - ", "\n", markdown)
  markdown
})

#' @rdname as.markdown
setMethod("as.markdown", "pressPartition", function(.Object, meta=c("text_newspaper", "text_date"), cpos=FALSE){
  articles <- apply(.Object@cpos, 1, function(row) {
    cposSeries <- c(row[1]:row[2])
    textStruc <- CQI$cpos2struc(.Object@corpus, ".text", row[1])
    pStrucs <- CQI$cpos2struc(.Object@corpus, ".p_type", cposSeries)
    chunks <- split(cposSeries, pStrucs)
    pTypes <- CQI$struc2str(.Object@corpus, ".p_type", as.numeric(names(chunks)))
    article <- Map(
      function(pType, chunk){
        tokens <- getTokenStream(
          matrix(c(chunk[1], chunk[length(chunk)]), nrow=1),
          corpus=.Object@corpus, pAttribute="word", encoding=.Object@encoding,
          cpos=cpos
        )
        if (cpos == TRUE) tokens <- .wrapTokens(tokens)
        para <- paste(tokens, collapse=" ")
        # Encoding(para) <- .Object@encoding
        #para <- as.utf8(para)
        para
      },
      pTypes, chunks
    )
    metaInformation <- sapply(
      meta,
      function(x) {
        retval <- CQI$struc2str(.Object@corpus, x, textStruc)
        Encoding(retval) <- .Object@encoding
        retval <- as.utf8(retval)
        retval
      })
    metaInformation <- paste(metaInformation, collapse=", ") # string will be converted to UTF-8
    article <- c(
      meta=metaInformation,
      article)
    .formattingFactory <- list(
      meta = function(x) paste("### ", x, sep=""),
      title = function(x) paste("## ", x, sep=""),
      teaser = function(x) paste("_", x, "_\n", sep=""),
      body = function(x) paste(x, "\n", sep=""),
      highlight = function(x) paste("_", x, "_\n", sep=""),
      headline = function(x) paste("# ", x, sep="")
    )
    formattedArticle <- lapply(
      c(1:length(article)),
      function(x) .formattingFactory[[names(article)[x]]](article[[x]])
    )
    markdown <- paste(formattedArticle, collapse="\n\n")
    # markdown <- as.utf8(markdown)
    markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
    markdown
  })
  paste(c("\n", unlist(articles)), collapse='\n* * *\n')
})


