#' @include partition_class.R partitionBundle_class.R
NULL

#' Generate html from object.
#' 
#' Prepare a html document to inspect the full text.
#' 
#' If param \code{charoffset} is \code{TRUE}, character offset positions will be
#' added to tags that embrace tokens. This may be useful, if exported html document
#' is annotated with a tools that stores annotations with character offset positions.
#' 
#' @param object the object the fulltext output will be based on
#' @param x object of class \code{html} to print
#' @param meta metadata for output, if NULL (default), the s-attributes defining
#'   a partition will be used
#' @param cpos logical, if \code{TRUE} (default), all tokens will be wrapped by 
#'   elements with id attribute indicating corpus positions
#' @param charoffset logical, if \code{TRUE}, character offset positions are
#'   added to elements embracing tokens
#' @param verbose logical, whether to be verbose
#' @param filename the filename
#' @param cutoff maximum number of tokens to decode from token stream, passed
#'   into \code{as.markdown}
#' @param type the partition type
#' @param i if object is a \code{kwic}-object, the index of the concordance for
#'   which the fulltext is to be generated
#' @param ... further parameters that are passed into \code{as.markdown}
#' @rdname html-method
#' @aliases show,html-method
#' @examples
#' use("polmineR")
#' P <- partition("REUTERS", places = "argentina")
#' H <- html(P)
#' H # show full text in viewer pane
#' 
#' # html-method can be used in a pipe
#' if (require("magrittr")){
#'   partition("REUTERS", places = "argentina") %>% html()
#' }
#' 
#' # use html-method to get from concordance to full text
#' K <- kwic("REUTERS", query = "barrels")
#' html(K, i = 1)
#' html(K, i = 2)
#' for (i in 1:length(K)) {
#'   show(html(K, i = i))
#'   if (interactive()){
#'     userinput <- readline("press 'q' to quit or any other key to continue")
#'     if (userinput == "q") break
#'   }
#' }
setGeneric("html", function(object, ...){standardGeneric("html")})


#' @rdname html-method
setMethod("html", "character", function(object){
  if (!requireNamespace("markdown", quietly = TRUE)){
    stop("package 'markdown' is not installed, but necessary for this function")
  }
  mdFilename <- tempfile(fileext = ".md")
  htmlFile <- tempfile(fileext = ".html")
  cat(object, file = mdFilename)
  markdown::markdownToHTML(file = mdFilename, output = htmlFile)  
  htmlFile
})


#' @importFrom xml2 read_html xml_find_all xml_text xml_parent xml_attr xml_attr<- xml_name
#' @importFrom stringi stri_extract_all_boundaries
#' @rdname html-method
.addCharacterOffset <- function(x){
  
  # check that required dependencies are present
  if (!requireNamespace("xml2", quietly = TRUE)) stop("package 'xml2' missing")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("package 'htmltools' missing")
  
  doc <- read_html(x)
  textnodes <- xml_find_all(doc, xpath = "//body//text()")
  nodetext <- sapply(textnodes, xml_text)
  nchar_textnodes <- sapply(nodetext, nchar)
  no_linesplit <- sapply(stri_extract_all_boundaries(nodetext), function(x) length(grep("\\n", x)))
  nchar_textnodes <- nchar_textnodes - ifelse(no_linesplit > 0, 1, 0)
  offset <- c(0, cumsum(nchar_textnodes)[1:(length(nchar_textnodes) - 1)])
  dummy <- lapply(
    1:length(textnodes),
    function(i){
      if (!grepl("^\\s*$", nodetext[i])){
        parent <- xml2::xml_parent(textnodes[[i]])
        if (xml_name(parent) == "span"){
          xml_attr(parent, "left") <- as.character(offset[i])
          xml_attr(parent, "right") <- offset[i] + nchar(nodetext[i])
        }
      }
      return(NULL)
    }
  )
  return( as.character(doc) )
}



#' @rdname html-method
#' @exportMethod html
#' @docType methods
setMethod(
  "html", "partition",
  function(object, meta = NULL, cpos = TRUE, verbose = FALSE, cutoff = NULL, charoffset = FALSE, ...){
    if (!requireNamespace("markdown", quietly = TRUE) && requireNamespace("htmltools", quietly = TRUE)){
      stop("package 'markdown' is not installed, but necessary for this function")
    }
    if (is.null(meta)) meta <- names(object@sAttributes)
    if (all(meta %in% sAttributes(object)) != TRUE) warning("not all sAttributes provided as meta are available")
    
    .message("generating markdown", verbose = verbose)
    markdown <- as.markdown(object, meta = meta, cpos = cpos, cutoff = cutoff, ...)
    
    .message("markdown to html", verbose = verbose)
    # the css for tooltips is included in the html document by default,
    # even if this causes some (minimal) overhead.
    # The default stylesheet (markdown.css) needs to be included explicitly,
    # so that it is not lost.
    css <- paste(c(
      readLines(getOption("markdown.HTML.stylesheet")),
      readLines(system.file("css", "tooltips.css", package = "polmineR"))
      ), collapse = "\n", sep = "\n"
    )
    doc <- markdown::markdownToHTML(text = markdown, stylesheet = css)
    
    if (charoffset) doc <- .addCharacterOffset(doc)
    
    htmltools::HTML(doc)
  }
)

#' @docType methods
#' @rdname html-method
setMethod("html", "partitionBundle", function(object, filename = c(), type = "debate"){
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
    .message("using metadata from template: ", paste(metadataDef, collapse = " / "), verbose = verbose)
    if (length(metadataDef) > 0){
      .message("enriching", verbose = verbose)
      object <- enrich(object, meta = metadataDef)
    }
  } else {
    metadataDef <- object@metadata
  }
  partitionToRead <- partition(
    object@corpus,
    def = lapply(setNames(metadataDef, metadataDef), function(x) object@table[[x]][i]),
    type = type
  )
  .message("generating html", verbose = verbose)
  fulltext <- polmineR::html(partitionToRead, meta = metadataDef, cpos = TRUE)
  .message("generating highlights", verbose = verbose)
  tabSubset <- object@cpos[which(object@cpos[["hit_no"]] == i)]
  cposContext <- tabSubset[which(tabSubset[["position"]] != 0)][["cpos"]]
  cposNode <- tabSubset[which(tabSubset[["position"]] == 0)][["cpos"]]
  fulltext <- highlight(
    fulltext,
    highlight = list(yellow = cposContext, lightgreen = cposNode)
  )
  fulltext
})
