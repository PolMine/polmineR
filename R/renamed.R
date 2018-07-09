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
