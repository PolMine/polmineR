#' @include size.R s_attributes.R kwic.R count.R

#' @title Access corpus on OpenCPU server.
#' 
#' @description A CWB corpus can be hosted on an OpenCPU (see \url{www.opencpu.org}) server.
#' The results of polmineR functions and methods that are executed on the remote
#' machine can be processed in a local R session.
#' 
#' @details The whereabouts of an OpenCPU server can be stated in an environment variable
#' "OPENCPU_SERVER". Environment variables for R sessions can be set easily in
#' the .Renviron file. A convenient way to do this is to call
#' \code{usethis::edit_r_environ()}.
#' @slot corpus Length-one character vector, name of corpus on remote machine.
#' @slot server The URL (can be IP address) of the OpenCPU server.
#' @slot type The type of the corpus (e.g. "plpr").
#' @slot encoding The encoding of the corpus.
#' @param .Object Name of a corpus.
#' @param x Name of a corpus.
#' @param query A query.
#' @param ... Further arguments passed.
#' @name opencpu
#' @rdname opencpu
#' @aliases opencpu remote_corpus remote_corpus-class
#' @seealso To instantiate an object of class \code{remote_corpus}, the
#'   \code{corpus}-class can be used (by providing argument \code{server}), see
#'   respective documentation.
#' @examples
#' \dontrun{
#' REUTERS <- corpus("REUTERS", server = Sys.getenv("OPENCPU_SERVER"))
#' count(REUTERS, query = "oil")
#' size(REUTERS)
#' kwic(REUTERS, query = "oil")
#' 
#' GERMAPARL <- new("remote_corpus", corpus = "GERMAPARL", server = Sys.getenv("OPENCPU_SERVER"))
#' s_attributes(GERMAPARL)
#' size(x = GERMAPARL)
#' count(GERMAPARL, query = "Integration")
#' kwic(GERMAPARL, query = "Islam")
#' }
#' @exportClass remote_corpus
#' @docType class
setClass(
  "remote_corpus",
  slots = c(
    corpus = "character",
    server = "character",
    type = "character",
    encoding = "character"
  )
)

.exec_on_opencpu <- function(fn, server, method = c("json", "protobuf"), ...){
  if (method == "json"){
    resp <- httr::POST(
      url = sprintf("http:/%s/ocpu/library/polmineR/R/%s/json", server, fn),
      body = list(...), 
      encode = 'json'
    )
    y <- jsonlite::fromJSON(rawToChar(resp$content))
    return(y)
  } else if (method == "protobuf"){
    stop("not yet implemented")
  }
  
}


#' @rdname opencpu
setMethod("count", "remote_corpus", function(.Object, query){
  y <- .exec_on_opencpu(fn = "count", method = "json", server = .Object@server, .Object = .Object@corpus, query = query)
  if (is.null(query)){
    return( data.table::data.table(y) )
  } else {
    return(y)
  }
})


#' @rdname opencpu
setMethod("kwic", "remote_corpus", function(.Object, ...){
  resp <- httr::POST(
    url = sprintf("http:/%s/ocpu/library/polmineR/R/kwic/pb", .Object@server),
    body = RProtoBuf::serialize_pb(c(.Object = .Object@corpus, list(...)), NULL),
    httr::add_headers("Content-Type" = "application/protobuf")
  )
  httr::stop_for_status(resp)
  output <- RProtoBuf::unserialize_pb(resp$content)
  output
})


#' @rdname opencpu
setMethod("size", "remote_corpus", function(x){
  .exec_on_opencpu(fn = "size", method = "json", server = x@server, x = x@corpus)
})


#' @rdname opencpu
setMethod("s_attributes", "remote_corpus", function(.Object){
  .exec_on_opencpu(fn = "s_attributes", method = "json", server = .Object@server, .Object = .Object@corpus)
})

