#' @noRd
setGeneric("as.phrases", function(.Object, ...) standardGeneric("as.phrases"))


#' @details If \code{.Object} is an object of class \code{ngrams}, the
#'   \code{as.phrases}-method will interpret the ngrams as CQP queries, 
#'   look up the matching corpus positions and return an \code{phrases}
#'   object.
#' @param .Object Input object, either a \code{ngrams} or a \code{matrix} object.
#' @param ... Arguments passed into internal call of \code{cpos} method.
#' @param enc Encoding of the corpus.
#' @param x A \code{phrases} class object.
#' @param p_attribute The positional attribute (p-attribute) to decode.
#' @param phrases A \code{phrases} class object.
#' @rdname phrases-class
#' @aliases as.phrases
#' @export
#' @examples
#' # Derive phrases object from an ngrams object
#' 
#' reuters_phrases <- ngrams("REUTERS", p_attribute = "word", n = 2L) %>%
#'   pmi(observed = count("REUTERS", p_attribute = "word")) %>%
#'   subset(ngram_count >= 5L) %>%
#'   subset(1:25) %>%
#'   as.phrases()
#' 
#' phr <- as.character(reuters_phrases, p_attribute = "word")
#' 
setMethod("as.phrases", "ngrams", function(.Object, ...){
  cols <- paste(.Object@p_attribute, 1L:.Object@n, sep = "_")
  args <- c(
    list(fmt = paste(rep('"%s"', times = .Object@n), collapse = " ")),
    as.list(.Object@stat[, cols, with = FALSE])
  )
  queries <- do.call(sprintf, args)
  query_check_results <- check_cqp_query(queries)
  if (isFALSE(all(query_check_results))){
    queries <- queries[query_check_results]
    warning("Queries dropped that are not valid CQP queries:", table(query_check_results)[["FALSE"]])
  }
  regions_matrix <- cpos(.Object@corpus, query = queries,  cqp = TRUE, check = FALSE, ...)
  as.phrases(regions_matrix, corpus = .Object@corpus, enc = .Object@encoding)
})


#' @export
#' @rdname phrases-class
#' @examples 
#' # Derive phrases from explicitly stated CQP queries
#' 
#' cqp_phrase_queries <- c(
#'   '"oil" "revenue"',
#'   '"Sheikh" "Aziz"',
#'   '"Abdul" "Aziz"',
#'   '"Saudi" "Arabia"',
#'   '"oil" "markets"'
#' )
#' reuters_phrases <- cpos("REUTERS", cqp_phrase_queries, p_attribute = "word") %>%
#'   as.phrases(corpus = "REUTERS", enc = "latin1")
#'   
#' @details If \code{.Object} is a \code{matrix}, the \code{as.phrases}-method
#'   will initialize a \code{phrases} object. The corpus and the encoding of the
#'   corpus will be assigned to the object.
setMethod("as.phrases", "matrix", function(.Object, corpus, enc = encoding(corpus)){
  new(
    "phrases",
    cpos = .Object,
    corpus = corpus,
    encoding = if (missing(enc)) encoding(corpus) else enc,
    size = sum(.Object[,2] - .Object[,1] + 1L)
  )
})


#' @export
#' @rdname phrases-class
#' @details Applying the \code{as.character}-method on a \code{phrases} object
#'   will return the decoded regions, concatenated using an underscore as
#'   seperator.
setMethod("as.character", "phrases", function(x, p_attribute){
  tokens <- get_token_stream(x@cpos, x@corpus, p_attribute = p_attribute, encoding = x@encoding)
  splitvec <-  cut(
    1L:length(tokens),
    breaks = c(1L, cumsum(x@cpos[,2] - x@cpos[,1] + 1L)),
    include.lowest = TRUE
  )
  unname(sapply(split(tokens, splitvec), function(toks) paste(toks, collapse = "_")))
})


#' @details The \code{concatenate_phrases} function takes a \code{data.table}
#'   (argument \code{dt}) as input and concatenates phrases in successive rows
#'   into a phrase.
#' @param dt A \code{data.table}.
#' @param col If \code{.Object} is a \code{data.table}, the column to concatenate.
#' @param corpus A length-one \code{character} vector, the corpus ID of the corpus
#'   from which regions / the \code{data.table} representing a decoded corpus is derived.
#' @examples 
#' # Use the concatenate_phrases() function on a data.table
#'  
#' lexical_units_cqp <- c(
#'   '"Deutsche.*" "Bundestag.*"',
#'   '"sozial.*" "Gerechtigkeit"',
#'   '"Ausschuss" "für" "Arbeit" "und" "Soziales"',
#'   '"soziale.*" "Marktwirtschaft"',
#'   '"freiheitliche.*" "Grundordnung"'
#' )
#' 
#' phr <- cpos("GERMAPARLMINI", query = lexical_units_cqp, cqp = TRUE) %>%
#'   as.phrases(corpus = "GERMAPARLMINI", enc = "word")
#' 
#' dt <- corpus("GERMAPARLMINI") %>%
#'   decode(p_attribute = "word", s_attribute = character(), to = "data.table") %>%
#'   concatenate_phrases(phrases = phr, col = "word")
#'   
#' dt[word == "Deutschen_Bundestag"]
#' dt[word == "soziale_Marktwirtschaft"]
#'
#' @export concatenate_phrases
#' @rdname phrases-class
concatenate_phrases <- function(dt, phrases, col){
  if (is.null(phrases)) warning("Argument 'regions' of concatenate,data.table-method is NULL: Mission may fail.")
  dt[, "keep" := TRUE]
  mwe <- as.character(phrases, p_attribute = col) # multi-word expressions
  dt[match(phrases@cpos[,1], dt[["cpos"]]), (col) := mwe]
  drop_cpos <- as.vector(unlist(apply(phrases@cpos, 1, function(row) (row[1] + 1L):row[2])))
  dt[match(drop_cpos, dt[["cpos"]]), "keep" := FALSE]
  dt[dt[["keep"]] == TRUE][, "keep" := NULL]
}

