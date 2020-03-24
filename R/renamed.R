#' Renamed Functions
#' 
#' These functions have been renamed in order to have a consistent coding style
#' that follows the snake_case convention. The "old" function still work to
#' maintain backwards compatiblity.
#' 
#' @name renamed
#' @rdname renamed
#' @param ... argument that are passed to the renamed function
NULL

#' @export sAttributes
#' @rdname renamed
sAttributes <- function(...) s_attributes(...)

#' @export pAttributes
#' @rdname renamed
pAttributes <- function(...) p_attributes(...)

#' @export getTokenStream
#' @rdname renamed
getTokenStream <- function(...) get_token_stream(...)

#' @export getTerms
#' @rdname renamed
getTerms <- function(...) terms(...)

#' @export getEncoding
#' @rdname renamed
getEncoding <- function(...) registry_get_encoding(...)

#' @export partitionBundle
#' @rdname renamed
partitionBundle <- function(...) partition_bundle(...)

#' @export as.partitionBundle
#' @rdname renamed
as.partitionBundle <- function(...) as.partition_bundle(...)

#' @export setTemplate
#' @rdname renamed
setTemplate <- function(...) set_template(...)

#' @export getTemplate
#' @rdname renamed
getTemplate <- function(...) get_template(...)

#' @rdname renamed
setMethod("corpus", "textstat", function(.Object){
  .Defunct(new = "get_corpus", package = "polmineR")
  get_corpus(.Object)
})

#' @rdname renamed
setMethod("corpus", "bundle", function(.Object){
  .Defunct(new = "get_corpus", package = "polmineR")
  get_corpus(.Object)
})


#' @rdname renamed
#' @param .Object A \code{kwic} object.
setMethod("corpus", "kwic", function(.Object){
  .Defunct(new = "get_corpus", package = "polmineR")
  get_corpus(.Object)
})


#' @title Defunct methods and functions.
#' @description Methods and functions not in use any more or that have been
#'   superseded by renamed functions.
#' 
#' @param ... Any arguments that may be passed into the defunct function/method.
#' @export browse
#' @rdname polmineR-defunct
#' @name polmineR-defunct
browse <- function(...) .Defunct(new = "html", package = "polmineR")


#' @details The RefClass `Corpus` has been superseded by the S4 `corpus`-class
#'   and adherent methods.
#' @rdname polmineR-defunct
#' @export Corpus
Corpus <- R6Class(
  
  "Corpus",
  
  public = list(
    
    corpus = NULL,
    registryDir = NULL,
    dataDir = NULL,
    encoding = NULL,
    cpos = NULL,
    p_attribute = NULL,
    s_attributes = NULL,
    size = NULL,
    stat = data.table(),
    
    initialize = function(corpus, p_attribute = NULL, s_attributes = NULL){
      
      stopifnot(is.character(corpus), length(corpus) == 1)
      if (!corpus %in% polmineR::corpus()[["corpus"]]) warning("corpus may not be available")
      self$corpus <- corpus
      
      self$registryDir <- Sys.getenv("CORPUS_REGISTRY")
      self$dataDir <- registry_get_home(corpus)
      self$encoding <- registry_get_encoding(corpus)
      self$size <- size(corpus)
      
      if (!is.null(p_attribute)){
        stopifnot(p_attribute %in% p_attributes(corpus))
        self$count(p_attribute)
      }
      
      if (!is.null(s_attributes)){
        dts <- lapply(
          s_attributes,
          function(s_attr){
            dt <- RcppCWB::s_attribute_decode(
              corpus = corpus,
              data_dir = registry_get_home(corpus),
              s_attribute = s_attr,
              encoding = registry_get_encoding(corpus),
              method = "R"
            )
            setkeyv(as.data.table(dt), c("cpos_left", "cpos_right"))
          }
        )
        dt <- dts[[1L]]
        setnames(dt, old = "value", new = s_attributes[1])
        if (length(s_attributes) > 1L){
          for (i in 2L:(length(s_attributes))){
            dt <- dt[ dts[[i]] ]
            setnames(dt, old = "value", new = s_attributes[i])
          }
        }
        
        self$cpos <- as.matrix(dt[, 1:2])
        self$s_attributes <- dt[, 3:ncol(dt)]
        self$s_attributes[["struc"]] <- 0:(nrow(self$s_attributes) - 1)
        setcolorder(self$s_attributes, neworder = c(ncol(self$s_attributes), 1:(ncol(self$s_attributes) - 1)))
      }
      
      invisible(self)
    }
  )
)
