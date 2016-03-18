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
#' @param tooltips tooltips for highlighted text
#' @rdname partition-class
#' @exportMethod html
#' @docType methods
#' @aliases html html-method html,partition-method show,html-method
setMethod("html", "partition", function(object, meta=NULL, highlight=list(), cqp=FALSE, tooltips=NULL, cpos=FALSE, verbose=FALSE, ...){
  if (requireNamespace("markdown", quietly=TRUE) && requireNamespace("htmltools", quietly=TRUE)){
    if (is.null(meta)) meta <- slot(get("session", '.GlobalEnv'), 'meta')
    if (all(meta %in% sAttributes(object)) != TRUE) warning("not all sAttributes provided as meta are available")
    if (verbose == TRUE) message("... enriching partition with metadata")
    object <- enrich(object, meta=meta, verbose=FALSE)
    if (verbose == TRUE) message("... generating markdown")
    if (any(cqp) == TRUE) cpos <- TRUE
    markdown <- as.markdown(object, meta, cpos=cpos, ...)
    markdown <- paste(
      paste('## Corpus: ', object@corpus, '\n* * *\n\n'),
      markdown,
      '\n* * *\n',
      collapse="\n"
    )
    if (verbose == TRUE) message("... markdown to html")
    if (is.null(tooltips)){
      htmlDoc <- markdown::markdownToHTML(text=markdown)
    } else {
      markdown_css <- scan(
        getOption("markdown.HTML.stylesheet"),
        what="character", sep="\n", quiet=TRUE
        )
      tooltips_css <- scan(
        system.file("css", "tooltips.css", package="polmineR"),
        what="character", sep="\n", quiet=TRUE
        )
      css <- paste(c(markdown_css, tooltips_css), collapse="\n")
      htmlDoc <- markdown::markdownToHTML(text=markdown, stylesheet=css)
    }
    
    if (length(highlight) > 0) {
      if (length(cqp) == 1){
        if (cqp == FALSE){
          if (verbose == TRUE) message("... highlighting regular expressions")
          htmlDoc <- highlight(htmlDoc, highlight=highlight, tooltips=tooltips)
        } else if (cqp == TRUE){
          if (verbose == TRUE) message("... highlighting CQP queries")
          htmlDoc <- highlight(object, htmlDoc, highlight=highlight, tooltips=tooltips)
        }
      } else if (length(cqp) == length(highlight)){
        if (any(!cqp)){
          htmlDoc <- highlight(
            htmlDoc,
            highlight=highlight[which(cqp == FALSE)],
            tooltips=tooltips[which(cqp == FALSE)]
          )
        }
        if (any(cqp)){
          htmlDoc <- highlight(
            object, htmlDoc,
            highlight=highlight[which(cqp == TRUE)],
            tooltips=tooltips[which(cqp == TRUE)]
          )
        }
      } else {
        message("length of cqp needs to be 1 or identical with the length of highlight")
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


