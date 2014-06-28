#' @include partition.R
NULL

#' Convert partition into html
#' 
#' A partition is converted into a html file and displayed in a browser
#'   
#' @param object a partition object
#' @param filename filename for the html file, if NULL (default), a temporary file is created
#' @param type the type of html to be generated
#' @aliases html html-method html,partition-method
#' @rdname html
#' @exportMethod html
setMethod("html", "partition", function(object, filename=NULL, type="debate"){
  if (length(object@metadata) == 0) warning("metadata missing!")
  markdown <- .partition2markdown(object, type)
  markdown <- paste(
    paste('## Excerpt from corpus', object@corpus, '\n* * *\n'),
    markdown,
    '\n* * *\n',
    collapse="\n")
  if (is.null(filename)) {
    htmlFile <- .markdown2tmpfile(markdown)
  } else {
    cat(markdown, file=filename)    
  }
  if (is.null(filename)) browseURL(htmlFile)
})
