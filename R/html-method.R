#' @include partition-class.R partitionCluster-class.R
NULL

setGeneric("html", function(object, ...){standardGeneric("html")})


#' @rdname as.markdown
setMethod("html", "character", function(object){
  if (requireNamespace("markdown", quietly=TRUE)){
    mdFilename <- tempfile(fileext=".md")
    htmlFile <- tempfile(fileext=".html")
    cat(object, file=mdFilename)
    markdown::markdownToHTML(file=mdFilename, output=htmlFile)  
  } else {
    warning("package 'markdown' is not installed, but necessary for this function")
    stop()
  }
  htmlFile
})


# #' @docType methods
# #' @rdname partitionCluster-class
# setMethod("html", "partitionCluster", function(object, meta=NULL, from=1, to=10, filename=NULL, type="debate"){
#   for (i in from:to){
#     html(object@objects[[i]], meta=meta, filename=filename, type=type)
#   }
# })

#' @param meta metadata for output
#' @rdname partition-class
#' @exportMethod html
#' @docType methods
#' @aliases html html-method html,partition-method show,html-method
setMethod("html", "partition", function(object, meta=NULL){
  if (requireNamespace("markdown", quietly=TRUE) && requireNamespace("htmltools", quietly=TRUE)){
    if (is.null(meta)) meta <- slot(get("session", '.GlobalEnv'), 'metadata')
    if (all(meta %in% sAttributes(object)) != TRUE) warning("not all sAttributes provided as meta are available")
    object <- enrich(object, meta=meta, verbose=FALSE)
    markdown <- as.markdown(object, meta)
    markdown <- paste(
      paste('## Corpus: ', object@corpus, '\n* * *\n\n'),
      markdown,
      '\n* * *\n',
      collapse="\n"
    )
    htmlDoc <- markdown::markdownToHTML(text=markdown)
    htmlDoc <- htmltools::HTML(htmlDoc)
  } else {
    warning("package 'markdown' is not installed, but necessary for this function")
    stop()
  }
  htmlDoc
})

#' @docType methods
#' @rdname partitionCluster-class
setMethod("html", "partitionCluster", function(object, filename=c(), type="debate"){
  markdown <- paste(lapply(object@objects, function(p) as.markdown(p, type)), collapse="\n* * *\n")
  markdown <- paste(
    paste('## Excerpt from corpus', object@corpus, '\n* * *\n'),
    markdown,
    '\n* * *\n',
    collapse="\n")
  if (is.null(filename)) {
    htmlFile <- html(markdown)
  } else {
    cat(markdown, file=filename)    
  }
  if (is.null(filename)) browseURL(htmlFile)
})


