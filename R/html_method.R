#' @include partition_class.R partitionBundle_class.R
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



#' @param meta metadata for output
#' @param highlight list with regex to be highlighted
#' @rdname partition-class
#' @exportMethod html
#' @docType methods
#' @aliases html html-method html,partition-method show,html-method
setMethod("html", "partition", function(object, meta=NULL, highlight=list(), cqp=FALSE, cpos=FALSE, verbose=FALSE, ...){
  if (requireNamespace("markdown", quietly=TRUE) && requireNamespace("htmltools", quietly=TRUE)){
    if (is.null(meta)) meta <- slot(get("session", '.GlobalEnv'), 'meta')
    if (all(meta %in% sAttributes(object)) != TRUE) warning("not all sAttributes provided as meta are available")
    if (verbose == TRUE) message("... enriching partition with metadata")
    object <- enrich(object, meta=meta, verbose=FALSE)
    if (verbose == TRUE) message("... generating markdown")
    markdown <- as.markdown(object, meta, cpos=cpos, ...)
    markdown <- paste(
      paste('## Corpus: ', object@corpus, '\n* * *\n\n'),
      markdown,
      '\n* * *\n',
      collapse="\n"
    )
    if (verbose == TRUE) message("... markdown to html")
    htmlDoc <- markdown::markdownToHTML(text=markdown)
    if (length(highlight) > 0) {
      if (cqp == FALSE){
        if (verbose == TRUE) message("... highlighting regular expressions")
        htmlDoc <- highlight(htmlDoc, highlight=highlight)
      } else if (cqp == TRUE){
        if (verbose == TRUE) message("... highlighting CQP queries")
        htmlDoc <- highlight(object, htmlDoc, highlight=highlight)
      }
    }
    htmlDocFinal <- htmltools::HTML(htmlDoc)
  } else {
    stop("package 'markdown' is not installed, but necessary for this function")
  }
  htmlDocFinal
})

#' @docType methods
#' @rdname partitionBundle-class
setMethod("html", "partitionBundle", function(object, filename=c(), type="debate"){
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


