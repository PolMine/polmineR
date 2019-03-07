#' @include size.R s_attributes.R kwic.R count.R

#' @title Access corpus on OpenCPU server.
#' 
#' @description A CWB corpus can be hosted on an OpenCPU (see
#'   \url{www.opencpu.org}) server. The results of polmineR functions and
#'   methods that are executed on the remote machine can be processed in a local
#'   R session.
#' @details A limited set of methods of the \code{polmineR} package is exposed
#'   to be executed on a remote OpenCPU server. See list of methods in the Usage
#'   section of this documentation entry. Note that the three dots (\code{...})
#'   are used to pass arguments. Hence, it is necessary to state the name of all
#'   arguments to be passed explicity.
#' @details The whereabouts of an OpenCPU server can be stated in an environment
#'   variable "OPENCPU_SERVER". Environment variables for R sessions can be set
#'   easily in the .Renviron file. A convenient way to do this is to call
#'   \code{usethis::edit_r_environ()}.
#' @slot server The URL (can be IP address) of the OpenCPU server.
#' @param .Object Name of a corpus .
#' @param x Name of a corpus.
#' @param method Whether to use Protocol Buffers ("protobuf") or JSON ("json") for 
#' transferring data to and from OpenCPU server. Defaults to "protobuf".
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
#' GERMAPARL <- corpus("GERMAPARL", server = Sys.getenv("OPENCPU_SERVER"))
#' s_attributes(GERMAPARL)
#' size(x = GERMAPARL)
#' count(GERMAPARL, query = "Integration")
#' kwic(GERMAPARL, query = "Islam")
#' 
#' p <- partition(GERMAPARL, year = 2000)
#' s_attributes(p, s_attribute = "year")
#' size(p)
#' kwic(p, query = "Islam", meta = "date")
#' }
#' @exportClass remote_corpus
#' @docType class
setClass(
  "remote_corpus",
  slots = c(server = "character"),
  contains = "corpus"
)


setAs(from = "corpus", to = "remote_corpus", def = function(from){
  y <- new("remote_corpus")
  for (x in slotNames(from)) slot(y, x) <- slot(from, x)
  y
})

setAs(from = "remote_corpus", to = "corpus", def = function(from){
  y <- new("corpus")
  for (x in slotNames(y)) slot(y, x) <- slot(from, x)
  y
})



#' @exportClass remote_partition
#' @rdname opencpu
setClass(
  "remote_partition",
  slots = c(server = "character"),
  contains = "partition"
)

setAs(from = "partition", to = "remote_partition", def = function(from){
  y <- new("remote_partition")
  for (x in slotNames(from)) slot(y, x) <- slot(from, x)
  y
})

setAs(from = "remote_partition", to = "partition", def = function(from){
  y <- new("partition")
  for (x in slotNames(y)) slot(y, x) <- slot(from, x)
  y
})



#' @rdname opencpu
#' @details \code{ocpu_exec} will execute function/method \code{fn} on a OpenCPU
#'   server (specified by argument \code{server}), using as a \code{method} to
#'   pass data either "protobuf" or "json" ("protobuf" is much more flexible and
#'   is the default), using three dots (\code{...}) to pass arguments.
#' @param fn Name of the function/method to execute on remote server (length-one
#'   \code{character} vector).
#' @param server The IP/URL of the remote OpenCPU server.
#' @param ... Further arguments passed into the method/function call.
#' @export ocpu_exec
ocpu_exec <- function(fn, server, method = "protobuf", ...){
  if (method == "json"){
    # using RProtoBuf offers much more flexibility and is the default way to access
    # the remote corpus. The 'json'-method remains, just in case it may be useful in 
    # the future
    resp <- httr::POST(
      url = sprintf("http:/%s/ocpu/library/polmineR/R/%s/json", server, fn),
      body = list(...), 
      encode = 'json'
    )
    y <- jsonlite::fromJSON(rawToChar(resp$content))
    return(y)
  } else if (method == "protobuf"){
    resp <- httr::POST(
      url = sprintf("http:/%s/ocpu/library/polmineR/R/%s/pb", server, fn),
      body = RProtoBuf::serialize_pb(list(...), NULL),
      httr::add_headers("Content-Type" = "application/protobuf")
    )
    httr::stop_for_status(resp)
    y <- RProtoBuf::unserialize_pb(resp$content)
    return(y)
  }
}


#' @rdname opencpu
setMethod("count", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "count", server = .Object@server, .Object = .Object@corpus, ...)
})


#' @rdname opencpu
setMethod("kwic", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "kwic", server = .Object@server, .Object = .Object@corpus, ...)
})


#' @rdname opencpu
setMethod("size", "remote_corpus", function(x){
  ocpu_exec(fn = "size", server = x@server, x = x@corpus)
})


#' @rdname opencpu
setMethod("s_attributes", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "s_attributes", server = .Object@server, .Object = .Object@corpus, ...)
})

#' @examples 
#' \dontrun{
#' GERMAPARL <- corpus("GERMAPARLMINI", server = Sys.getenv("OPENCPU_SERVER"))
#' subset(GERMAPARL, date == "2009-11-10")
#' }
#' @rdname opencpu
setMethod("subset", "remote_corpus", function(x, ...){
  expr <- substitute(...)
  args <- list(x = as(x, "corpus"), subset = expr)
  body <- RProtoBuf::serialize_pb(args, NULL)
  resp <- httr::POST(
    url = sprintf("http:/%s/ocpu/library/base/R/subset/pb", x@server),
    body = body,
    httr::add_headers("Content-Type" = "application/protobuf")
  )
  httr::stop_for_status(resp)
  y <- RProtoBuf::unserialize_pb(resp$content)
})


#' @rdname opencpu
setMethod("partition", "remote_corpus", function(.Object, ...){
  p <- ocpu_exec(fn = "partition", server = .Object@server, .Object = .Object@corpus, ...)
  y <- as(p, "remote_partition")
  y@server <- .Object@server
  y
})


#' @rdname opencpu
setMethod("partition", "remote_partition", function(.Object, ...){
  p <- ocpu_exec(fn = "partition", server = .Object@server, .Object = as(.Object, "partition", ...))
  y <- as(p, "remote_partition")
  y@server <- .Object@server
  y
})

#' @rdname opencpu
setMethod("s_attributes", "remote_partition", function(.Object, ...){
  ocpu_exec(fn = "s_attributes", server = .Object@server, .Object = as(.Object, "partition"), ...)
})

#' @rdname opencpu
setMethod("size", "remote_partition", function(x){
  ocpu_exec(fn = "size", server = x@server, x = as(x, "partition"))
})

#' @rdname opencpu
setMethod("kwic", "remote_partition", function(.Object, ...){
  ocpu_exec(fn = "kwic", server = .Object@server, .Object = as(.Object, "partition"), ...)
})

