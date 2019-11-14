#' Execute code on OpenCPU server
#' 
#' \code{ocpu_exec} will execute a function/method \code{fn} on an OpenCPU server
#' (specified by argument \code{server}), using three dots (\code{...}) to pass
#' arguments. It is the worker of methods defined for \code{remote_corpus},
#' \code{remote_subcorpus} and \code{remote_partition} objects.
#'   
#' @param fn Name of the function/method to execute on remote server (length-one
#'   \code{character} vector).
#' @param server The IP/URL of the remote OpenCPU server.
#' @param user A username required for authentication, if necessary.
#' @param password A password required for authentication, if necessary.
#' @param do.call Logical, if \code{TRUE}, the function \code{fn} is passed into
#'   a call of \code{do.call}, which offers some flexibility.
#' @param ... Further arguments passed into the method/function call.
#' @export ocpu_exec
#' @rdname ocpu_exec
#' @aliases opencpu
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
ocpu_exec <- function(fn, server, user = NULL, password = NULL, do.call = FALSE, ...){
  if (!requireNamespace("httr", quietly = TRUE))
    stop("To access a remote corpus, package 'httr' is required, but it is not yet installed.")
  if (!requireNamespace("curl", quietly = TRUE))
    stop("To access a remote corpus, package 'curl' is required, but it is not yet installed.")
  if (!requireNamespace("protolite", quietly = TRUE))
    stop("To access a remote corpus, package 'protolite' is required, but it is not yet installed.")

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
  if (length(user) == 0L && length(password) == 0L){
    resp <- httr::POST(url = url, body = body)
  } else {
    resp <- httr::POST(url = url, body = body, httr::authenticate(user = user, password = password))
  }
  httr::stop_for_status(resp)
  y <- protolite::unserialize_pb(resp$content)
}

