#' @rdname phrases-class
#' @param .Object bla
#' @param ... bla
#' @param enc bla
#' @param x bla
#' @param p_attribute bla
#' @param phrases bla
#' @rdname phrases-class
setGeneric("as.phrases", function(.Object, ...) standardGeneric("as.phrases"))


#' @export
#' @rdname phrases-class
#' @examples
#' reuters_phrases <- ngrams("REUTERS", p_attribute = "word", n = 2L) %>%
#'   pmi(observed = count("REUTERS", p_attribute = "word")) %>%
#'   subset(ngram_count >= 5L) %>%
#'   subset(1:25) %>%
#'   as.phrases()
#' 
#' phr <- as.character(reuters_phrases, p_attribute = "word")
setMethod("as.phrases", "ngrams", function(.Object, ...){
  cols <- paste(.Object@p_attribute, 1L:.Object@n, sep = "_")
  args <- c(
    list(fmt = paste(rep('"%s"', times = .Object@n), collapse = " ")),
    as.list(.Object@stat[, cols, with = FALSE])
  )
  queries <- do.call(sprintf, args)
  regions_matrix <- cpos(.Object@corpus, query = queries,  cqp = TRUE, ...)
  as.phrases(regions_matrix, corpus = .Object@corpus, enc = .Object@encoding)
})


#' @export
#' @rdname phrases-class
#' @examples 
#' cqp_phrase_queries <- c(
#'   '"oil" "revenue"',
#'   '"Sheikh" "Aziz"',
#'   '"Abdul" "Aziz"',
#'   '"Saudi" "Arabia"',
#'   '"oil" "markets"'
#' )
#' reuters_phrases <- cpos("REUTERS", cqp_phrase_queries, p_attribute = "word") %>%
#'   as.phrases(corpus = "REUTERS", enc = "latin1")
setMethod("as.phrases", "matrix", function(.Object, corpus, enc){
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
setMethod("as.character", "phrases", function(x, p_attribute){
  tokens <- get_token_stream(x@cpos, x@corpus, p_attribute = p_attribute, x@encoding)
  splitvec <-  cut(
    1L:length(tokens),
    breaks = c(1L, cumsum(x@cpos[,2] - x@cpos[,1] + 1L)),
    include.lowest = TRUE
  )
  unname(sapply(split(tokens, splitvec), function(x) paste(x, collapse = "_")))
})


#' @param dt A \code{data.table}.
#' @param col If \code{.Object} is a \code{data.table}, the column to concatenate.
#' @param corpus A length-one \code{character} vector, the corpus ID of the corpus
#'   from which regions / the \code{data.table} representing a decoded corpus is derived.
#' @examples 
#' lexical_units_cqp <- c(
#'   '"Deutsche.*" "Bundestag.*"',
#'   '"sozial.*" "Gerechtigkeit"',
#'   '"Ausschuss" "für" "Arbeit" "und" "Soziales"',
#'   '"soziale.*" "Marktwirtschaft"',
#'   '"freiheitliche.*" "Grundordnung"'
#' )
#' phrases_regions <- cpos("GERMAPARLMINI", query = lexical_units_cqp, cqp = TRUE)
#' phr <- as.phrases(phrases_regions, corpus = "GERMAPARLMINI", enc = "word")
#' 
#' dt <- decode("GERMAPARLMINI", p_attribute = "word", s_attribute = character(), to = "data.table")
#' y <- concatenate_phrases(dt, phrases = phr, col = "word")
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

