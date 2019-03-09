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
#' @param subset Logical expression indicating elements or rows to keep.
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
#' 
#' GERMAPARL <- corpus("GERMAPARLMINI", server = Sys.getenv("OPENCPU_SERVER"))
#' s_attrs <- s_attributes(GERMAPARL, s_attribute = "date")
#' sc <- subset(GERMAPARL, date == "2009-11-10")
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


#' @exportClass remote_subcorpus
#' @rdname opencpu
setClass(
  "remote_subcorpus",
  slots = c(server = "character"),
  contains = "subcorpus"
)


setAs(from = "subcorpus", to = "remote_subcorpus", def = function(from){
  y <- new("remote_subcorpus")
  for (x in slotNames(from)) slot(y, x) <- slot(from, x)
  y
})

setAs(from = "remote_subcorpus", to = "subcorpus", def = function(from){
  y <- new("subcorpus")
  for (x in slotNames(y)) slot(y, x) <- slot(from, x)
  y
})


#' @rdname opencpu
setMethod("count", "remote_subcorpus", function(.Object, ...){
  ocpu_exec(fn = "count", server = .Object@server, do.call = FALSE, .Object = as(.Object, "subcorpus"), ...)
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
#'   server (specified by argument \code{server}), using three dots (\code{...})
#'   to pass arguments.
#' @param fn Name of the function/method to execute on remote server (length-one
#'   \code{character} vector).
#' @param server The IP/URL of the remote OpenCPU server.
#' @param do.call Logical, if \code{TRUE}, the function \code{fn} is passed into
#'   a call of \code{do.call}, which offers some flexibility.
#' @param ... Further arguments passed into the method/function call.
#' @export ocpu_exec
#' @examples
#' \dontrun{
#' # Get polmineR version installed on remote server
#' ocpu_exec(
#'   fn = "packageVersion",
#'   server = Sys.getenv("OPENCPU_SERVER"),
#'   do.call = TRUE,
#'   pkg = "polmineR"
#' )
#' 
#' ocpu_exec(
#'   fn = "subset",
#'   server = Sys.getenv("OPENCPU_SERVER"),
#'   do.call = TRUE,
#'   x = iris,
#'   subset = substitute(Sepal.Width > 4.0)
#' )
#' }
ocpu_exec <- function(fn, server, do.call = FALSE, ...){
  if (!do.call){
    url <- sprintf("%s/ocpu/library/polmineR/R/%s/pb", server, fn)
    args <- list(...)
  } else {
    url <- sprintf("%s/ocpu/library/base/R/do.call/pb", server)
    args <- list(what = fn, args = list(...))
  }
  
  body <- lapply(
    list(...),
    function(x)
      if (class(x) == "call")
        deparse(x)
    else
      curl::form_data(protolite::serialize_pb(x), "application/protobuf")
  )
  resp <- httr::POST(url = url, body = body)
  httr::stop_for_status(resp)
  y <- protolite::unserialize_pb(resp$content)
}


#' @rdname opencpu
setMethod("count", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "count", server = .Object@server, do.call = FALSE, .Object = .Object@corpus, ...)
})


#' @rdname opencpu
setMethod("kwic", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "kwic", server = .Object@server, do.call = FALSE, .Object = .Object@corpus, ...)
})


#' @rdname opencpu
setMethod("size", "remote_corpus", function(x){
  ocpu_exec(fn = "size", server = x@server, do.call = FALSE, x = x@corpus)
})


#' @rdname opencpu
setMethod("s_attributes", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "s_attributes", server = .Object@server, do.call = FALSE, .Object = .Object@corpus, ...)
})

#' @rdname opencpu
setMethod("subset", "remote_corpus", function(x, subset){
  expr <- substitute(subset)
  y <- ocpu_exec(fn = "subset", server = x@server, do.call = FALSE, x = as(x, "corpus"), subset = expr)
  as(y, "remote_subcorpus")
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

