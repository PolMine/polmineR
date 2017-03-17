#' @include partition_class.R partitionBundle_class.R
NULL

#' restore fulltext as html
#' 
#' @param object the object
#' @param cqp logical
#' @param cpos logical
#' @param verbose logical
#' @param filename the filename
#' @param cutoff maximum number of tokens to decode from token stream
#' @param type the partition type
#' @param i to be checked
#' @param ... further parameters
#' @rdname html-method
#' @aliases show,html-method
setGeneric("html", function(object, ...){standardGeneric("html")})


#' @rdname html-method
setMethod("html", "character", function(object){
  if (requireNamespace("markdown", quietly = TRUE)){
    mdFilename <- tempfile(fileext = ".md")
    htmlFile <- tempfile(fileext = ".html")
    cat(object, file = mdFilename)
    markdown::markdownToHTML(file = mdFilename, output = htmlFile)  
  } else {
    stop("package 'markdown' is not installed, but necessary for this function")
  }
  htmlFile
})



#' @param meta metadata for output
#' @param highlight list with regex to be highlighted
#' @param tooltips tooltips for highlighted text
#' @rdname html-method
#' @exportMethod html
#' @docType methods
setMethod(
  "html", "partition",
  function(
    object, meta = getOption("polmineR.meta"),
    highlight = list(),
    cqp = FALSE, tooltips = NULL, cpos = FALSE, verbose = FALSE, cutoff = NULL, ...
    ){
  if (requireNamespace("markdown", quietly=TRUE) && requireNamespace("htmltools", quietly=TRUE)){
    if (all(meta %in% sAttributes(object)) != TRUE) warning("not all sAttributes provided as meta are available")
    if (verbose) message("... enriching partition with metadata")
    object <- enrich(object, meta = meta, verbose=FALSE)
    if (verbose) message("... generating markdown")
    if (any(cqp)) cpos <- TRUE
    markdown <- as.markdown(object, meta = meta, cpos = cpos, cutoff = cutoff, ...)
    markdown <- paste(
      paste('## Corpus: ', object@corpus, '\n* * *\n\n'),
      markdown,
      '\n* * *\n',
      collapse="\n"
    )
    if (verbose == TRUE) message("... markdown to html")
    if (is.null(tooltips)){
      htmlDoc <- markdown::markdownToHTML(text = markdown)
    } else {
      markdown_css <- scan(
        getOption("markdown.HTML.stylesheet"),
        what = "character", sep = "\n", quiet=TRUE
        )
      tooltips_css <- scan(
        system.file("css", "tooltips.css", package="polmineR"),
        what = "character", sep = "\n", quiet = TRUE
        )
      css <- paste(c(markdown_css, tooltips_css), collapse = "\n")
      htmlDoc <- markdown::markdownToHTML(text = markdown, stylesheet = css)
    }
    
    if (length(highlight) > 0) {
      if (length(cqp) == 1){
        if (cqp == FALSE){
          if (verbose == TRUE) message("... highlighting regular expressions")
          htmlDoc <- highlight(htmlDoc, highlight = highlight, tooltips = tooltips)
        } else if (cqp == TRUE){
          if (verbose == TRUE) message("... highlighting CQP queries")
          htmlDoc <- highlight(object, htmlDoc, highlight = highlight, tooltips = tooltips)
        }
      } else if (length(cqp) == length(highlight)){
        if (any(!cqp)){
          htmlDoc <- highlight(
            htmlDoc,
            highlight = highlight[which(cqp == FALSE)],
            tooltips = tooltips[which(cqp == FALSE)]
          )
        }
        if (any(cqp)){
          htmlDoc <- highlight(
            object, htmlDoc,
            highlight = highlight[which(cqp == TRUE)],
            tooltips = tooltips[which(cqp == TRUE)]
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
#' @rdname html-method
setMethod("html", "partitionBundle", function(object, filename = c(), type="debate"){
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

#' @rdname html-method
setMethod("html", "kwic", function(object, i, type = NULL, verbose = FALSE){
  
  # getting metadata for all kwic lines is potentially not the fastes solution ...
  if (length(object@metadata) == 0){
    metadataDef <- getOption("polmineR.templates")[[object@corpus]][["metadata"]]
    if (verbose) message("... using metadata from template: ", metadataDef)
    if (length(metadataDef) > 0){
      if (verbose) message("... enriching")
      object <- enrich(object, meta = metadataDef)
    }
  }
  partitionToRead <- partition(
    object@corpus,
    def = lapply(setNames(object@metadata, object@metadata), function(x) object@table[[x]][i]),
    type = type
  )
  if (verbose) message("... generating html")
  fulltext <- html(partitionToRead, meta = object@metadata, cpos = TRUE)
  if (verbose) message("... generating highlights")
  fulltext <- highlight(
    fulltext,
    highlight = list(
      yellow = object@cpos[which(object@cpos[["hit_no"]] == i)][which(object@cpos[["position"]] != 0)][["cpos"]],
      lightgreen = object@cpos[which(object@cpos[["hit_no"]] == i)][which(object@cpos[["position"]] != 0)][["cpos"]]
    )
  )
  fulltext
})
