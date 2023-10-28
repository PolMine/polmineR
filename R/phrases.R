#' @noRd
setGeneric("as.phrases", function(.Object, ...) standardGeneric("as.phrases"))


#' @details If `.Object` is an object of class `ngrams`, the
#'   `as.phrases()`-method will interpret the ngrams as CQP queries, 
#'   look up the matching corpus positions and return an `phrases`
#'   object.
#' @param .Object Input object, either a `ngrams` or a `matrix` object.
#' @param enc Encoding of the corpus.
#' @param x A `phrases` class object.
#' @param p_attribute The positional attribute (p-attribute) to decode.
#' @param phrases A `phrases` class object.
#' @rdname phrases-class
#' @aliases phrases as.phrases
#' @export
#' @examples
#' \dontrun{
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' # Derive phrases object from an ngrams object
#' 
#' reuters_phrases <- ngrams("REUTERS", p_attribute = "word", n = 2L) %>%
#'   pmi(observed = count("REUTERS", p_attribute = "word")) %>%
#'   subset(ngram_count >= 5L) %>%
#'   subset(1:25) %>%
#'   as.phrases()
#' 
#' phr <- as.character(reuters_phrases, p_attribute = "word")
#' }
setMethod("as.phrases", "ngrams", function(.Object){
  # First, prepare data.table with token id representation of phrases to look up
  li <- lapply(
    paste(.Object@p_attribute, 1L:.Object@n, sep = "_"),
    function(colname){
      tokens <- as.corpusEnc(x = .Object@stat[[colname]], corpusEnc = .Object@encoding)
      cl_str2id(
        corpus = .Object@corpus, registry = .Object@registry_dir,
        p_attribute = .Object@p_attribute, str = tokens
      )
    }
  )
  id_dt <- as.data.table(li)
  
  
  # Anticipate whether memory will suffice
  cnt_file <- path(
    corpus_data_dir(.Object@corpus, registry = .Object@registry_dir),
    sprintf("%s.corpus.cnt", .Object@p_attribute)
  )
  cnt_file_size <- file.info(cnt_file)$size
  cnt <- readBin(con = cnt_file, what = integer(), size = 4L, n = cnt_file_size, endian = "big")
  if (sum(cnt[id_dt[[1]] + 1L]) > 2**31){
    warning("Table to detect ngrams will exceed memory that can be allocated. The problem is likely ", 
            "to result from very frequent tokens as first tokens of ngrams. More restrictive ",
            "filtering of ngrams is recommended.")
  }
  
  # Expand first token to corpus positions of initial token
  cpos_dt <- data.table(unique(li[[1]]))[, list(cpos = RcppCWB::cl_id2cpos(corpus = .Object@corpus, registry = .Object@registry_dir, p_attribute = .Object@p_attribute, id = .SD[["V1"]])), by = "V1", .SDcols = "V1"]
  # allow.cartesian = TRUE appropriate because several different ngrams may start with same token (id)
  y <- cpos_dt[id_dt, on = "V1", allow.cartesian = TRUE]   

  # Get id for 2nd, 3rd ... nth token after start corpus position and limit table to those matching the id
  # at the position
  for (i in 2L:.Object@n){
    nextid <- cpos2id(
      x = .Object, p_attribute = .Object@p_attribute,
      cpos = (y[["cpos"]] + i - 1L)
    )
    y <- y[y[[paste("V", i, sep = "")]] == nextid]
  }
  
  as.phrases(
    matrix(data = c(y[["cpos"]], (y[["cpos"]] + .Object@n - 1L)), ncol = 2), 
    corpus = .Object@corpus,
    enc = .Object@encoding
  )
})


#' @export
#' @rdname phrases-class
#' @examples 
#' # Derive phrases from explicitly stated CQP queries
#' 
#' \dontrun{
#' cqp_phrase_queries <- c(
#'   '"oil" "revenue";',
#'   '"Sheikh" "Aziz";',
#'   '"Abdul" "Aziz";',
#'   '"Saudi" "Arabia";',
#'   '"oil" "markets";'
#' )
#' reuters_phrases <- cpos("REUTERS", cqp_phrase_queries, p_attribute = "word") %>%
#'   as.phrases(corpus = "REUTERS", enc = "latin1")
#' }
#'   
#' @details If `.Object` is a `matrix`, the `as.phrases()`-method will
#'   initialize a `phrases` object. The corpus and the encoding of the corpus
#'   will be assigned to the object.
setMethod("as.phrases", "matrix", function(.Object, corpus, enc = encoding(corpus)){
  corpus_obj <- corpus(corpus)
  new(
    "phrases",
    cpos = .Object,
    corpus = corpus,
    registry_dir = corpus_obj@registry_dir,
    data_dir = corpus_obj@data_dir,
    info_file = corpus_obj@info_file,
    template = corpus_obj@template,
    encoding = if (missing(enc)) corpus_obj@encoding else enc,
    size = sum(.Object[,2] - .Object[,1] + 1L)
  )
})


#' @export
#' @rdname phrases-class
#' @details Applying the \code{as.character}-method on a \code{phrases} object
#'   will return the decoded regions, concatenated using an underscore as
#'   seperator.
setMethod("as.character", "phrases", function(x, p_attribute){
  tokens <- get_token_stream(x@cpos, corpus = x@corpus, p_attribute = p_attribute, encoding = x@encoding)
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
#' \dontrun{
#' #' lexical_units_cqp <- c(
#'   '"Deutsche.*" "Bundestag.*";',
#'   '"sozial.*" "Gerechtigkeit";',
#'   '"Ausschuss" "f.r" "Arbeit" "und" "Soziales";',
#'   '"soziale.*" "Marktwirtschaft";',
#'   '"freiheitliche.*" "Grundordnung";'
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
#' }  
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

