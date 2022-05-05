#' @include hits.R S4classes.R
NULL

#' Get corpus positions for a query or queries.
#' 
#' Get matches for a query in a CQP corpus (subcorpus, partition etc.),
#' optionally using the CQP syntax of the Corpus Workbench (CWB).
#' 
#' If the \code{cpos()}-method is applied on a \code{character} or
#' \code{partition} object, the result is a two-column \code{matrix} with the
#' regions (start end end corpus positions of the matches) for a query. CQP
#' syntax can be used. The encoding of the query is adjusted to conform to the
#' encoding of the CWB corpus. If there are not matches, \code{NULL} is
#' returned.
#' 
#' If the \code{cpos()}-method is called on a \code{matrix} object,  the cpos
#' matrix is unfolded, the return value is an integer vector with the individual
#' corpus positions.
#' 
#' If \code{.Object} is a \code{hits} object, an \code{integer} vector is
#' returned with the individual corpus positions.
#' 
#' @param .Object A length-one \code{character} vector indicating a CWB corpus, a
#'   \code{partition} object, or a \code{matrix} with corpus positions.
#' @param query A `character` vector providing one or multiple queries (token to
#'   look up, regular expression or CQP query). Token ids (i.e. `integer`
#'   values) are also accepted. If `query` is neither a regular expression nor a
#'   CQP query, a sanity check removes accidental leading/trailing whitespace,
#'   issuing a respective warning.
#' @param cqp Either logical (\code{TRUE} if query is a CQP query), or a function to
#'   check whether query is a CQP query or not (defaults to \code{is.cqp} auxiliary
#'   function).
#' @param regex Interpret \code{query} as a regular expression. 
#' @param check A \code{logical} value, whether to check validity of CQP query
#'   using \code{check_cqp_query}.
#' @param p_attribute The p-attribute to search. Needs to be stated only if query
#'   is not a CQP query. Defaults to \code{NULL}.
#' @param verbose A \code{logical} value, whether to show messages.
#' @param ... Used for reasons of backwards compatibility to
#'   process arguments that have been renamed (e.g. \code{pAttribute}).
#' @return Unless \code{.Object} is a \code{matrix}, the return value is a
#'   \code{matrix} with two columns.  The first column reports the left/starting
#'   corpus positions (cpos) of the hits obtained. The second column reports the
#'   right/ending corpus positions of the respective hit. The number of rows is
#'   the number of hits. If there are no hits, a \code{NULL} object is returned.
#' @exportMethod cpos
#' @rdname cpos-method
#' @name cpos
#' @importFrom data.table fread
#' @examples
#' use("polmineR")
#' 
#' # looking up single tokens
#' cpos("REUTERS", query = "oil")
#' corpus("REUTERS") %>% cpos(query = "oil")
#' corpus("REUTERS") %>% subset(grepl("saudi-arabia", places)) %>% cpos(query = "oil")
#' partition("REUTERS", places = "saudi-arabia", regex = TRUE) %>% cpos(query = "oil")
#' 
#' # using CQP query syntax
#' cpos("REUTERS", query = '"Saudi" "Arabia"')
#' corpus("REUTERS") %>% cpos(query = '"Saudi" "Arabia"')
#' corpus("REUTERS") %>%
#'   subset(grepl("saudi-arabia", places)) %>%
#'   cpos(query = '"Saudi" "Arabia"', cqp = TRUE)
#' partition("REUTERS", places = "saudi-arabia", regex = TRUE) %>%
#'   cpos(query = '"Saudi" "Arabia"', cqp = TRUE)
setGeneric("cpos", function(.Object, ... ) standardGeneric("cpos"))

#' @rdname cpos-method
setMethod("cpos", "corpus", function(.Object, query, p_attribute = getOption("polmineR.p_attribute"), cqp = is.cqp, regex = FALSE, check = TRUE, verbose = TRUE, ...){
  
  dots <- list(...)
  if (length(dots) == 1L){
    if (names(dots) == "pAttribute"){
      p_attribute <- dots[["pAttribute"]]
    } else {
      warning("An argument has been passed into the cpos()-method via three dots (...) that is unknown.")
    }
  } else if (length(dots) > 1L){
    warning("An argument has been passed into the cpos()-method via three dots (...) that is unknown.")
  }

  query <- as.corpusEnc(query, corpusEnc = .Object@encoding)
  if (is.function(cqp)) cqp <- cqp(query)
  if (length(cqp) > 1L) stop("length of cqp is more than 1, but needs to be exactly 1")
  if (!cqp) {
    
    .fn <- function(id){
      regions <- cl_id2cpos(
        corpus = .Object@corpus, registry = .Object@registry_dir,
        p_attribute = p_attribute, id = id
      )
      matrix(c(regions, regions), ncol = 2L)
    }

    hit_list <- lapply(
      query,
      function(q){
        
        if (is.character(q) && !regex){
          if (grepl("^\\s+", q) || grepl("\\s+$", q)){
            warning(
              sprintf(
                "Query '%s' includes leading and/or trailing whitespace. ",
                q
              ),
              "Surplus whitespace is considered to be accidental and will be removedv for token lookup."
            )
            q <- gsub("^\\s*(.*?)\\s*$", "\\1", q)
          }
        }
        
        regions <- try({
          if (is.character(q)){
            if (!regex){
              ids <- cl_str2id(
                corpus = .Object@corpus, registry = .Object@registry_dir,
                p_attribute = p_attribute, str = q
              )
            } else {
              ids <- cl_regex2id(
                corpus = .Object@corpus, registry = .Object@registry_dir,
                p_attribute = p_attribute, regex = q
              )
            }
          } else if (is.integer(q)){
            ids <- q
          } else {
            warning("Argument 'query' needs to be an integer value or a character vector.")
          }
          
          if (length(ids) == 1L){
            if (ids < 0L){ # CQP will return -1 or another negative value if there are no matches
              .message("no hits for query: ", q, verbose = verbose)
              return( NULL )
            } else {
              return(.fn(ids))
            }
          } else {
            return( do.call(rbind, lapply(ids, .fn)) )
          }
        })
      }
    )
  } else if (cqp) {
    hit_list <- lapply(
      query,
      function(q){
        if (check) if (!check_cqp_query(q)) stop("Aborting - CQP query does not pass check and may cause a crash.")
        if (!RcppCWB::cqp_is_initialized()) cqp_initialize()
        cqp_query(corpus = .Object@corpus, query = q)
        regions <- try(cqp_dump_subcorpus(corpus = .Object@corpus), silent = TRUE)
        if (is(regions)[1] == "try-error"){
          .message("no hits for query: ", q, verbose = verbose)
          return( NULL )
        } else if (!is.null(regions)) {
          return( matrix(regions[,c(1L,2L)], ncol = 2L) )
        } else {
          .message("no hits for query: ", q, verbose = verbose)
          return( NULL )
        }
      }
    )
  }
  hits <- do.call(rbind, hit_list)
  if (is.null(hits)) return( hits )
  if (nrow(hits) == 0L) invisible( NULL ) else hits
})

#' @rdname cpos-method
setMethod("cpos", "character", function(.Object, query, p_attribute = getOption("polmineR.p_attribute"), cqp = is.cqp, check = TRUE, verbose = TRUE, ...){
  cpos(.Object = corpus(.Object), query = query, p_attribute = p_attribute, cqp = cqp, check = check, verbose = verbose, ...)
})

  
#' @rdname cpos-method
setMethod("cpos", "slice", function(.Object, query, cqp = is.cqp, check = TRUE, p_attribute = getOption("polmineR.p_attribute"), verbose = TRUE, ...){
  hits <- cpos(
    as(.Object, "corpus"),
    query = query, cqp = cqp, check = check,
    p_attribute = p_attribute,
    verbose = verbose, ...
  )
  
  if (!is.null(hits)){
    if (length(.Object@s_attribute_strucs) > 0L){
      # The incoming .Object may be a partition/subcorpus object that has been generated
      # from a corpus object. In this case, the slot s_attribute_strucs is an empty character
      # vector, and no filtering will be performed. This is used by the coocurrences-method
      # that is implemented for the partition class, but not for the corpus class.
      struc_hits <- cl_cpos2struc(
        corpus = .Object@corpus,  registry = .Object@registry_dir,
        s_attribute = .Object@s_attribute_strucs, cpos = hits[,1]
      )
      hits <- hits[which(struc_hits %in% .Object@strucs),]
      if (is(hits)[1] == "integer") hits <- matrix(data = hits, ncol = 2L)
      if (nrow(hits) == 0L) hits <- NULL
    }
  }
  if (is(hits)[1] == "integer") hits <- matrix(hits, ncol = 2L)
  hits
})


#' @rdname cpos-method
setMethod("cpos", "partition", function(.Object, query, cqp = is.cqp, check = TRUE, p_attribute = getOption("polmineR.p_attribute"), verbose = TRUE, ...){
  callNextMethod(.Object = .Object, query = query, cqp = cqp, check = check, p_attribute = p_attribute, verbose = verbose, ...)
})

#' @rdname cpos-method
setMethod("cpos", "subcorpus", function(.Object, query, cqp = is.cqp, check = TRUE, p_attribute = getOption("polmineR.p_attribute"), verbose = TRUE, ...){
  callNextMethod(.Object = .Object, query = query, cqp = cqp, check = check, p_attribute = p_attribute, verbose = verbose, ...)
})



#' @details. If \code{.Object} is a \code{matrix}, it is assumed to be a region
#' matrix, i.e. a two-column \code{matrix} with left and right corpus positions
#' in the first and second row, respectively. For many operations, such as
#' decoding the token stream, it is necessary to inflate the denoted regions
#' into a vector of all corpus positions referred to by the regions defined in
#' the matrix. The \code{cpos}-method for \code{matrix} objects will performs
#' this task robustly.
#' @rdname cpos-method
#' @importFrom RcppCWB ranges_to_cpos
setMethod("cpos", "matrix", function(.Object)
  ranges_to_cpos(.Object)
)

#' @rdname cpos-method
setMethod("cpos", "hits", function(.Object)
  cpos(as.matrix(.Object@stat[, c("cpos_left", "cpos_right")]))
)


#' @details If \code{.Object} is \code{NULL}, the method will return an empty
#'   integer vector. Used internally to handle \code{NULL} objects that may be
#'   returned from the \code{cpos}-method if no matches are obtained for a
#'   query.
#' @rdname cpos-method
setMethod("cpos", "NULL", function(.Object) integer())

