#' @include S4classes.R
NULL

#' Get markdown-formatted full text of a partition.
#' 
#' The method is the worker behind the \code{read}-method, which will be called
#' usually to reconstruct the full text of a \code{partition} and read it. The
#' \code{as.markdown}-method can be customized for different classes inheriting
#' from the \code{partition}-class.
#' 
#' @param .Object The object to be converted, a \code{partition}, or a class
#'   inheriting from \code{partition}, such as \code{plpr_partition}.
#' @param meta The metainformation (s-attributes) to be displayed.
#' @param cpos A \code{logical} value, whether to add cpos as ids in span elements.
#' @param interjections A \code{logical} value, whether to format interjections.
#' @param cutoff The maximum number of tokens to reconstruct, to avoid that full text is
#' excessively long.
#' @param template A template for formating output.
#' @param verbose A \code{logical} value, whether to output messages.
#' @param ... further arguments
#' @rdname as.markdown
#' @exportMethod as.markdown
#' @examples
#' use("polmineR")
#' P <- partition("REUTERS", places = "argentina")
#' as.markdown(P)
#' as.markdown(P, meta = c("id", "places"))
#' if (interactive()) read(P, meta = c("id", "places"))
setGeneric("as.markdown", function(.Object, ...) standardGeneric("as.markdown"))

# vectorized sprintf is considerably faster than shiny::span,
# an alternative that could be considered
.tagTokens <- function(tokens){
  if (is.null(names(tokens))) {
    return( sprintf('<span token="%s" class="fulltext">%s</span>', tokens, tokens) )
  } else {
    return( sprintf('<span id="%s" token="%s" class="fulltext">%s</span>', names(tokens), tokens, tokens) )
  }
}


.as.markdown <- function(.Object, corpus, meta = NULL, cpos = FALSE, cutoff = NULL, ...){
  corpusEncoding <- registry_get_encoding(corpus)
  
  # generate metainformation
  documentStruc <- CQI$cpos2struc(corpus, get_template(corpus)[["document"]][["sAttribute"]], .Object[1])

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
    get_template(corpus)[["document"]][["format"]][1],
    metaInformation,
    get_template(corpus)[["document"]][["format"]][2],
    sep = ""
    )
  
  cposSeries <- .Object[1]:.Object[2]
  pStrucs <- CQI$cpos2struc(corpus, get_template(corpus)[["paragraphs"]][["sAttribute"]], cposSeries)
  chunks <- split(cposSeries, pStrucs)
  pTypes <- CQI$struc2str(corpus, get_template(corpus)[["paragraphs"]][["sAttribute"]], as.numeric(names(chunks)))
  bodyList <- Map(
    function(pType, chunk){
      tokens <- get_token_stream(
        chunk, corpus = corpus, p_attribute = "word",
        encoding = corpusEncoding, cpos = cpos, cutoff = cutoff
      )
      tokens <- .tagTokens(tokens)
      paste(
        get_template(corpus)[["paragraphs"]][["format"]][[pType]][1],
        paste(tokens, collapse = " "),
        get_template(corpus)[["paragraphs"]][["format"]][[pType]][2],
        sep = ""
      )
    },
    pTypes, chunks
  )
  article <- c(metaInformation, unlist(bodyList))
  markdown <- paste(article, collapse = "\n\n")
  markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
  markdown
}


#' @rdname as.markdown
setMethod(
  "as.markdown", "partition",
  function(
    .Object,
    meta = getOption("polmineR.meta"), template = get_template(.Object),
    cpos = TRUE, cutoff = NULL, verbose = FALSE,
    ...
  ){
    .message("as.markdown", verbose = verbose)
    # ensure that template requested is available
    if (is.null(template)){
      stop("template needed for formatting a partition of corpus ", .Object@corpus , " is missing, use set_template()")
    }
    if (is.null(template[["paragraphs"]])){
      .message("generating paragraphs (no template)", verbose = verbose)
      tokens <- get_token_stream(.Object, p_attribute = "word", cpos = cpos, cutoff = cutoff, ...)
      tokens <- .tagTokens(tokens)
      tokens <- paste(tokens, collapse = " ")
      rawTxt <- paste(tokens, sep = "\n")
      txt <- gsub("(.)\\s([,.:!?])", "\\1\\2", rawTxt)
    } else {
      .message("generating paragraphs (template for paras)", verbose = verbose)
      articles <- apply(
        .Object@cpos, 1,
        function(row) .as.markdown(row, corpus = .Object@corpus, meta = meta, cutoff = cutoff, ...)
        )
      txt <- paste(c("\n", unlist(articles)), collapse = '\n* * *\n')
    }
    txt
    metaInfo <- paste(
      sapply(meta, function(x) sprintf("%s: %s", x, paste(s_attributes(.Object, x), collapse = "|"))),
      collapse = " // "
    )
    corpusInfo <- paste("## Corpus: ", .Object@corpus, "\n\n", sep = "")
    header <- paste(corpusInfo, paste("### ", metaInfo), "\n\n", sep = "")
    txt <- paste(header, txt, '\n', collapse = "\n")
    txt
  })

#' @rdname as.markdown
setMethod("as.markdown", "plpr_partition", function(.Object, meta = NULL, template = get_template(.Object), cpos = FALSE, interjections = TRUE, cutoff = NULL, ...){
  # in the function call, meta is actually not needed, required by the calling function
  if (is.null(meta)) meta <- template[["metadata"]]
  if (interjections){
    maxNoStrucs <- .Object@strucs[length(.Object@strucs)] - .Object@strucs[1] + 1L
    if (maxNoStrucs != length(.Object@strucs)){
      .Object@strucs <- .Object@strucs[1]:.Object@strucs[length(.Object@strucs)]
      # fill regions matrix to include interjections
      .Object@cpos <- RcppCWB::get_region_matrix(
        corpus = .Object@corpus, s_attribute = .Object@s_attribute_strucs,
        registry = Sys.getenv("CORPUS_REGISTRY"), strucs = .Object@strucs
      )
      
    }
  }
  
  # detect where a change of metainformation occurs
  metadata <- as.matrix(s_attributes(.Object, s_attribute = meta)) # somewhat slow
  if (length(.Object@strucs) > 1L){
    metaChange <- sapply(2L:nrow(metadata), function(i) !all(metadata[i,] == metadata[i - 1L,]))
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
    1L:nrow(metadata),
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
      tokens <- get_token_stream(
        matrix(.Object@cpos[i,], nrow = 1),
        corpus = .Object@corpus, p_attribute = "word", encoding = .Object@encoding,
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
