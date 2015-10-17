#' get the encoding of a corpus
#' 
#' @param .Object the corpus name
#' @rdname getEncoding-method
#' @name getEncoding
#' @exportMethod getEncoding
setGeneric("getEncoding", function(.Object) standardGeneric("getEncoding"))

#' @rdname getEncoding-method
setMethod("getEncoding", "character", function(.Object){
  registry <- scan(
    file=file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(.Object)),
    sep="\n",
    what="character",
    quiet=TRUE
  )
  encodingLine <- registry[grep('charset\\s*=\\s*"', registry)]
  encoding <- sub('^.*charset\\s*=\\s*"(.+?)".*$', "\\1", encodingLine)
  encoding <- toupper(encoding)
  if (!encoding %in% iconvlist()){
    warning('Please check encoding in the registry file (charset="..." provides unknown encoding) or provide encoding explicitly')
  }
  return(tolower(encoding))
})
