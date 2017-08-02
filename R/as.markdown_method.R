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
#' @param template a template for formatting output
#' @param ... further arguments
#' @rdname as.markdown
#' @exportMethod as.markdown
setGeneric("as.markdown", function(.Object, ...) standardGeneric("as.markdown"))

# vectorized sprintf is considerably faster than shiny::span,
# an alternative that could be considered
.tagTokens <- function(tokens){
  if (is.null(names(tokens))) {
    return( sprintf('<span token="%s">%s</span>', tokens, tokens) )
  } else {
    return( sprintf('<span id="%s" token="%s">%s</span>', names(tokens), tokens, tokens) )
  }
}


#' @rdname as.markdown
setMethod("as.markdown", "numeric", function(.Object, corpus, meta = NULL, cpos = FALSE, cutoff = NULL, ...){
  corpusEncoding <- RegistryFile$new(corpus)$getEncoding()
  
  # generate metainformation
  documentStruc <- CQI$cpos2struc(corpus, getTemplate(corpus)[["document"]][["sAttribute"]], .Object[1])

  metaInformation <- sapply(
    meta,
    function(x) {
      retval <- CQI$struc2str(corpus, x, documentStruc)
      Encoding(retval) <- corpusEncoding
      as.nativeEnc(retval, from = corpusEncoding)
    })
  names(metaInformation) <- meta
  
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
      tokens <- .tagTokens(tokens)
      paste(
        getTemplate(corpus)[["paragraphs"]][["format"]][[pType]][1],
        paste(tokens, collapse = " "),
        getTemplate(corpus)[["paragraphs"]][["format"]][[pType]][2],
        sep = ""
      )
    },
    pTypes, chunks
  )
  article <- c(metaInformation, unlist(bodyList))
  markdown <- paste(article, collapse = "\n\n")
  markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
  markdown
})


#' @rdname partition_class
setMethod(
  "as.markdown", "partition",
  function(
    .Object,
    meta = getOption("polmineR.meta"), template = getTemplate(.Object),
    cpos = TRUE, cutoff = NULL, verbose = FALSE,
    ...
  ){
    if (verbose) message("... as.markdown")
    # ensure that template requested is available
    if (is.null(template)){
      stop("template needed for formatting a partition of corpus ", .Object@corpus , " is missing, use setTemplate()")
    }
    if (is.null(template[["paragraphs"]])){
      if (verbose) message("... generating paragraphs (no template)")
      tokens <- getTokenStream(.Object, pAttribute = "word", cpos = cpos, cutoff = cutoff, ...)
      tokens <- .tagTokens(tokens)
      tokens <- paste(tokens, collapse = " ")
      rawTxt <- paste(tokens, sep = "\n")
      txt <- gsub("(.)\\s([,.:!?])", "\\1\\2", rawTxt)
    } else {
      if (verbose) message("... generating paragraphs (template for paras)")
      articles <- apply(
        .Object@cpos, 1,
        function(row) as.markdown(row, corpus = .Object@corpus, meta = meta, cutoff = cutoff, ...)
        )
      txt <- paste(c("\n", unlist(articles)), collapse='\n* * *\n')
    }
    txt
  })

#' @rdname as.markdown
setMethod("as.markdown", "plprPartition", function(.Object, meta = NULL, template = getTemplate(.Object), cpos = FALSE, interjections = TRUE, cutoff = NULL, ...){
  # in the function call, meta is actually not needed, required by the calling function
  if (is.null(meta)) meta <- template[["metadata"]]
  if (interjections){
    maxNoStrucs <- .Object@strucs[length(.Object@strucs)] - .Object@strucs[1] + 1
    if (maxNoStrucs != length(.Object@strucs)){
      .Object@strucs <- .Object@strucs[1]:.Object@strucs[length(.Object@strucs)]
      # fill regions matrix to include interjections
      if (requireNamespace("polmineR.Rcpp", quietly = TRUE)){
        .Object@cpos <- polmineR.Rcpp::get_region_matrix(
          corpus = .Object@corpus, s_attribute = .Object@sAttributeStrucs,
          registry = Sys.getenv("CORPUS_REGISTRY"), struc = .Object@strucs
        )
        
      } else {
        .Object@cpos <- do.call(rbind, lapply(
          .Object@strucs,
          function(i) CQI$struc2cpos(.Object@corpus, .Object@sAttributeStrucs, i)
        ))
      }
    }
  }
  
  # detect where a change of metainformation occurs
  metadata <- as.matrix(sAttributes(.Object, sAttribute = meta)) # somewhat slow
  if (length(.Object@strucs) > 1){
    metaChange <- sapply(2:nrow(metadata), function(i) !all(metadata[i,] == metadata[i - 1,]))
    metaChange <- c(TRUE, metaChange)
  } else {
    metaChange <- TRUE
  }
  
  type <- CQI$struc2str(.Object@corpus, template[["speech"]][["sAttribute"]], .Object@strucs)
  
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
  
  markdown <- sapply(
    1:nrow(metadata),
    function(i) {
      meta <- ""
      if (metaChange[i] == TRUE) { 
        meta <- paste(metadata[i,], collapse=" | ", sep="")
        meta <- paste(
          template[["document"]][["format"]][1],
          meta,
          template[["document"]][["format"]][2],
          collapse = ""
        )
        meta <- as.corpusEnc(meta, corpusEnc = .Object@encoding)
      }
      tokens <- getTokenStream(
        matrix(.Object@cpos[i,], nrow = 1),
        corpus = .Object@corpus, pAttribute = "word", encoding = .Object@encoding,
        cpos = cpos
      )
    tokens <- .tagTokens(tokens)
    plainText <- paste(tokens, collapse = " ")
    plainText <- paste(
      template[["speech"]][["format"]][[type[i]]][1],
      plainText,
      template[["speech"]][["format"]][[type[i]]][1],
      sep = ""
      )
    paste(meta, plainText)
  })
  markdown <- paste(markdown, collapse = "\n\n")
  markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
  markdown <- gsub("\n - ", "\n", markdown)
  markdown
})
