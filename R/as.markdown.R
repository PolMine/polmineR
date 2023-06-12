#' @include S4classes.R
NULL

#' Get markdown-formatted full text of a partition.
#' 
#' The method is the worker behind the `read()`-method, which will be called
#' usually to reconstruct the full text of a `partition` and read it. The
#' `as.markdown()`-method can be customized for different classes inheriting
#' from the `partition`-class.
#' 
#' @param .Object The object to be converted, a `partition`, or a class
#'   inheriting from `partition`, such as `plpr_partition`.
#' @param meta The metainformation (s-attributes) to be displayed.
#' @param cpos A `logical` value, whether to add cpos as ids in span elements.
#' @param interjections A `logical` value, whether to format interjections.
#' @param cutoff The maximum number of tokens to reconstruct, to avoid that full
#'   text is excessively long.
#' @param template A template for formating output.
#' @param verbose A `logical` value, whether to output messages.
#' @param ... further arguments
#' @rdname as.markdown
#' @exportMethod as.markdown
#' @examples
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' P <- partition("REUTERS", places = "argentina")
#' as.markdown(P)
#' as.markdown(P, meta = c("id", "places"))
#' if (interactive()) read(P, meta = c("id", "places"))
setGeneric("as.markdown", function(.Object, ...) standardGeneric("as.markdown"))

# vectorized sprintf is considerably faster than shiny::span,
# an alternative that could be considered
.tagTokens <- function(tokens){
  
  # This is different from purging markdown doc from signs that would interfere
  # with markdown formatting syntax: We replace signs that would mess up 
  # opening and closing double quotes here.
  
  if ('"' %in% tokens) tokens[which(tokens == '"')] <- "'"
  if ('\u201e' %in% tokens) tokens[which(tokens == '\u201e')] <- "'"
  if ('\u201c' %in% tokens) tokens[which(tokens == '\u201c')] <- "'"
  
  if (is.null(names(tokens))){
    return(
      sprintf(
        '<span token="%s" class="fulltext">%s</span>',
        tokens,
        tokens
      )
    )
  } else {
    return(
      sprintf(
        '<span id="%s" token="%s" class="fulltext">%s</span>',
        names(tokens),
        tokens,
        tokens
      )
    )
  }
}



#' @rdname as.markdown
setMethod(
  "as.markdown", "partition",
  function(
    .Object, meta = getOption("polmineR.meta"), template = get_template(.Object),
    cpos = TRUE, cutoff = NULL, verbose = FALSE,
    ...
  ){
    as.markdown(
      .Object = as(.Object, "subcorpus"),
      meta = meta, template = template, cpos = cpos,
      cutoff = cutoff, verbose = verbose,
      ...
    )
})


#' @rdname as.markdown
setMethod(
  "as.markdown", "subcorpus",
  function(
    .Object,
    meta = getOption("polmineR.meta"), template = get_template(.Object),
    cpos = TRUE, cutoff = NULL, verbose = FALSE,
    ...
  ){
    .message("as.markdown", verbose = verbose)
    # ensure that template requested is available
    if (is.null(template)){
      cli_alert_warning(
        "No template available for corpus {.val {get_corpus(.Object)}}. Using default template."
      )
      template <- default_template
    }
    if (is.null(template[["paragraphs"]])){
      .message("generating paragraphs (no template)", verbose = verbose)
      tokens <- get_token_stream(
        .Object,
        p_attribute = "word", cpos = cpos,
        cutoff = cutoff, ...
      )
      tokens <- .tagTokens(tokens)
      tokens <- paste(tokens, collapse = " ")
      txt_raw <- paste(tokens, sep = "\n")
      txt <- gsub("(.)\\s([,.:!?])", "\\1\\2", txt_raw)
    } else {
      if (verbose) cli_alert_info("generating paragraphs (using template)")
      articles <- apply(
        .Object@cpos, 1L,
        function(row){
          # Previously, there was a check here whether template is NULL
          # but there is the initial check already!
          
          # generate metainformation
          doc_struc <- cl_cpos2struc(
            corpus = .Object@corpus, registry = .Object@registry_dir,
            s_attribute = template[["document"]][["sAttribute"]],
            cpos = row[1]
          )
          
          meta_values <- sapply(
            meta,
            function(x) {
              retval <- cl_struc2str(
                corpus = .Object@corpus, registry = .Object@registry_dir,
                s_attribute = x, struc = doc_struc
              )
              Encoding(retval) <- .Object@encoding
              as.nativeEnc(retval, from = .Object@encoding)
            }
          )
          names(meta_values) <- meta
          
          meta_values <- paste(meta_values, collapse = ", ") # string will be converted to UTF-8
          meta_values <- paste(
            template[["document"]][["format"]][1],
            meta_values,
            template[["document"]][["format"]][2],
            sep = ""
          )
          
          corpus_positions <- row[1]:row[2]
          para_strucs <- cl_cpos2struc(
            corpus = .Object@corpus, registry = .Object@registry_dir,
            s_attribute = template[["paragraphs"]][["sAttribute"]],
            cpos = corpus_positions
          )
          chunks <- split(corpus_positions, para_strucs)
          para_types <- cl_struc2str(
            corpus = .Object@corpus, registry = .Object@registry_dir,
            s_attribute = template[["paragraphs"]][["sAttribute"]],
            struc = as.integer(names(chunks))
          )
          body_li <- Map(
            function(p_type, chunk){
              tokens <- get_token_stream(
                chunk, corpus = .Object@corpus, p_attribute = "word",
                encoding = .Object@encoding, cpos = cpos, cutoff = cutoff
              )
              tokens <- .tagTokens(tokens)
              paste(
                template[["paragraphs"]][["format"]][[p_type]][1],
                paste(
                  stringi::stri_c(
                    ifelse(
                      c(TRUE, grepl("[,.:!?]", tokens[2L:length(tokens)])),
                      "",
                      " "
                    ),
                    tokens
                  ),
                  collapse = ""
                ),
                template[["paragraphs"]][["format"]][[p_type]][2],
                sep = ""
              )
            },
            para_types, chunks
          )
          article <- c(meta_values, unlist(body_li))
          md <- paste(article, collapse = "\n\n")
          md
        }
      )
      txt <- paste(c("\n", unlist(articles)), collapse = '\n* * *\n')
    }
    
    corpus_info <- paste("## Corpus: ", .Object@corpus, "\n\n", sep = "")
    txt <- paste(corpus_info, txt, '\n', collapse = "\n")
    txt
  })

#' @rdname as.markdown
setMethod("as.markdown", "plpr_partition", function(.Object, meta = NULL, template = get_template(.Object), cpos = FALSE, interjections = TRUE, cutoff = NULL, ...){
  as.markdown(as(.Object, "plpr_subcorpus"), meta = meta, template = template, cpos = cpos, interjections = interjections, cutoff = cutoff, ...)
})


#' @rdname as.markdown
#' @importFrom cli cli_alert_warning
setMethod("as.markdown", "plpr_subcorpus", function(.Object, meta = NULL, template = get_template(.Object), cpos = FALSE, interjections = TRUE, cutoff = NULL, ...){
  # in the function call, meta is actually not needed, required by the calling function
  
  if (is.null(template))
    cli_alert_warning(
      "No template available, generating fulltext output likely to fail."
    )
  
  if (is.null(meta)) meta <- unlist(unname(template[["metadata"]]))
  
  if (interjections){
    expected <- .Object@strucs[length(.Object@strucs)] - .Object@strucs[1] + 1L
    if (expected != length(.Object@strucs)){
      .Object@strucs <- .Object@strucs[1]:.Object@strucs[length(.Object@strucs)]
      # fill regions matrix to include interjections
      .Object@cpos <- RcppCWB::get_region_matrix(
        corpus = .Object@corpus, registry = .Object@registry_dir,
        s_attribute = .Object@s_attribute_strucs, strucs = .Object@strucs
      )
    }
  }
  
  # detect where a change of metainformation occurs (somewhat slow)
  metadata <- as.matrix(
    s_attributes(.Object, s_attribute = meta, unique = FALSE)
  ) 
  if (length(.Object@strucs) > 1L){
    meta_change <- sapply(
      2L:nrow(metadata),
      function(i) !all(metadata[i,] == metadata[i - 1L,])
    )
    meta_change <- c(TRUE, meta_change)
  } else {
    meta_change <- TRUE
  }
  
  type <- cl_struc2str(
    corpus = .Object@corpus,
    registry = .Object@registry_dir,
    s_attribute = template[["speech"]][["sAttribute"]],
    struc = .Object@strucs
  )
  
  if (is.numeric(cutoff)){
    beyond_cutoff <- which(cumsum(.Object@cpos[,2] - .Object@cpos[,1]) > cutoff)
    if (length(beyond_cutoff) > 0L){
      threshold <- min(beyond_cutoff)
      if (threshold > 1L){
        metadata <- metadata[1L:threshold,]
        .Object@cpos <- .Object@cpos[1L:threshold,]
      }
    }
  }
  
  markdown <- sapply(
    1L:nrow(metadata),
    function(i) {
      meta <- ""
      if (meta_change[i] == TRUE) { 
        meta <- paste(metadata[i,], collapse=" | ", sep = "")
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
        corpus = .Object@corpus,
        p_attribute = "word",
        encoding = .Object@encoding,
        cpos = cpos
      )
    tokens <- .tagTokens(tokens)
    plaintext <- paste(tokens, collapse = " ")
    plaintext <- paste(
      template[["speech"]][["format"]][[type[i]]][1],
      plaintext,
      template[["speech"]][["format"]][[type[i]]][1],
      sep = ""
      )
    paste(meta, plaintext)
  })
  markdown <- paste(markdown, collapse = "\n\n")
  markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
  markdown <- gsub("\n - ", "\n", markdown)
  markdown
})
