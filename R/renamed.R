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
