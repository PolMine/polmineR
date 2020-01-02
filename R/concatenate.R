#' Concatenate tokens to phrases
#' 
#' Concatenate sequences of tokens defined by regions to phrases.
#' 
#' @param .Object Either a \code{matrix} or a \code{data.table}.
#' @param regions A two-column \code{matrix} with corpus positions defining 
#'   regions.
#' @param col If \code{.Object} is a \code{data.table}, the column to concatenate.
#' @param corpus A length-one \code{character} vector, the coropus ID of the corpus
#'   from which regions / the \code{data.table} representing a decoded corpus is derived.
#' @param p_attribute The positional attribute for which tokens shall be decoded.
#' @param ... Further arguments that may be defined for the method.
#' @exportMethod concatenate
#' @docType methods
#' @rdname concatenate
#' @examples 
#' lexical_units_cqp <- c(
#'   '"Deutsche.*" "Bundestag.*"',
#'   '"sozial.*" "Gerechtigkeit"',
#'   '"Ausschuss" "für" "Arbeit" "und" "Soziales"',
#'   '"soziale.*" "Marktwirtschaft"',
#'   '"freiheitliche.*" "Grundordnung"'
#' )
#' phrases_regions <- cpos("GERMAPARLMINI", query = lexical_units_cqp, cqp = TRUE)
#' phrases <- concatenate(phrases_regions, corpus = "GERMAPARLMINI", p_attribute = "word")
#' 
#' dt <- decode("GERMAPARLMINI", p_attribute = "word", s_attribute = character(), to = "data.table")
#' y <- concatenate(dt, regions = phrases_regions, col = "word", corpus = "GERMAPARLMINI")
setGeneric("concatenate", function(.Object, ...) standardGeneric("concatenate") )


#' @exportMethod concatenate
#' @rdname concatenate
setMethod("concatenate", "data.table", function(.Object, regions, col, corpus){
  .Object[, "keep" := TRUE]
  if (is.null(regions)) warning("Argument 'regions' of concatenate,data.table-method is NULL: Mission may fail.")
  phrases <- concatenate(regions, corpus = corpus, p_attribute = col)
  .Object[match(regions[,1], .Object[["cpos"]]), (col) := phrases]
  drop_cpos <- as.vector(unlist(apply(regions, 1, function(row) (row[1] + 1L):row[2])))
  .Object[match(drop_cpos, .Object[["cpos"]]), "keep" := FALSE]
  .Object[.Object[["keep"]] == TRUE][, "keep" := NULL]
})


#' @exportMethod concatenate
#' @rdname concatenate
setMethod("concatenate", "matrix", function(.Object, corpus, p_attribute){
  tokens <- get_token_stream(.Object, corpus = corpus, p_attribute = p_attribute, encoding = encoding(corpus))
  splitvec <-  cut(
    1L:length(tokens),
    breaks = c(1L, cumsum(.Object[,2] - .Object[,1] + 1L)),
    include.lowest = TRUE
  )
  unname(sapply(split(tokens, splitvec), function(x) paste(x, collapse = "_")))
})
