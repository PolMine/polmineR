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
#' @param corpus A length-one \code{character} vector, the id of the corpus to be 
#'   queried.
#' @param restricted A \code{logical} value, whether credentials are required to
#'   access the data.
#' @param do.call Logical, if \code{TRUE}, the function \code{fn} is passed into
#'   a call of \code{do.call}, which offers some flexibility.
#' @param ... Arguments passed into the method/function call.
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
#' }
ocpu_exec <- function(fn, corpus, server, restricted = FALSE, do.call = FALSE, ...){
  if (!requireNamespace("httr", quietly = TRUE))
    stop("To access a remote corpus, package 'httr' is required. The 'httr' package is not installed.")
  if (!requireNamespace("curl", quietly = TRUE))
    stop("To access a remote corpus, package 'curl' is required, but it is not yet installed.")
  if (!requireNamespace("protolite", quietly = TRUE))
    stop("To access a remote corpus, package 'protolite' is required, but it is not yet installed.")

  url <- if (isFALSE(do.call)){
    sprintf("%s/ocpu/library/polmineR/R/%s/pb", server, fn)
  } else {
    sprintf("%s/ocpu/library/base/R/do.call/pb", server)
  }
  
  body <- lapply(
    list(...),
    function(x)
      if (class(x) == "call"){
        deparse(x)
      } else {
        curl::form_data(protolite::serialize_pb(x), "application/protobuf")
      }
  )
  if (isTRUE(restricted)){
    opencpu_registry <- Sys.getenv("OPENCPU_REGISTRY")
    if (identical(nchar(opencpu_registry), 0L)){
      stop("Access to corpora with restricted corpora requires that the environment variable 'OPENCPU_REGISTRY' is set.")
    }
    
    properties <- registry_get_properties(corpus = corpus, registry = opencpu_registry)
    resp <- httr::POST(
      url = url, body = body,
      httr::authenticate(user = properties[["user"]], password = properties[["password"]])
    )
    rm(properties)
  } else {
    resp <- httr::POST(url = url, body = body)
  }
  httr::stop_for_status(resp)
  protolite::unserialize_pb(resp$content)
}

