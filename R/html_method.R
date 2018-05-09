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
#' a partition will be used
#' @param sAttribute structural attributes that will be used to define the partition 
#' where the match occurred
#' @param cpos logical, if \code{TRUE} (default), all tokens will be wrapped by 
#'   elements with id attribute indicating corpus positions
#' @param beautify logical, if \code{TRUE}, whitespace before interpunctuation
#'   will be removed
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
#' if (interactive()) H # show full text in viewer pane
#' 
#' # html-method can be used in a pipe
#' if (require("magrittr")){
#'   H <- partition("REUTERS", places = "argentina") %>% html()
#'   # use html-method to get from concordance to full text
#'   K <- kwic("REUTERS", query = "barrels")
#'   H <- html(K, i = 1, sAttribute = "id")
#'   H <- html(K, i = 2, sAttribute = "id")
#'   for (i in 1:length(K)) {
#'     H <- html(K, i = i, sAttribute = "id")
#'     if (interactive()){
#'       show(H)
#'       userinput <- readline("press 'q' to quit or any other key to continue")
#'       if (userinput == "q") break
#'     }
#'   }
#' }
#' 
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
#' @importFrom xml2 xml_attrs xml_remove
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

#' @importFrom xml2 xml_find_first xml_text<-
.beautify <- function(x){
  doc <- xml2::read_html(x)
  lapply(
    c(",", ".", ":", ";", "!", "?"),
    function(interpunctuation){
      nodes <- xml2::xml_find_all(
        doc,
        xpath = sprintf("//span[starts-with(@token, '%s')]", interpunctuation)
        )
      if (length(nodes) >= 1){
        lapply(
          1:length(nodes),
          function(j){
            precedingTextNode <- xml_find_first(
              nodes[[j]],
              xpath = "./preceding-sibling::text()[1]"
            )
            if (!is.na(xml_text(precedingTextNode))){
              if (xml_text(precedingTextNode) == " ") xml_text(precedingTextNode) <- ""
            }
          }
        )
      }
    }
  )
  
  # remove continuing double quotes
  nodes <- xml_find_all(doc, xpath = sprintf('//span[@token = "%s"]', "''"))
  if (length(nodes) >= 2){
    cpos <- as.integer(sapply(lapply(nodes, xml_attrs), function(x) x["id"]))
    for (i in 2:length(cpos)){
      if (cpos[i-1] + 1 == cpos[i]){
        nodeToRemove <- xml_find_first(doc, xpath = sprintf('//span[@id = "%d"]', cpos[i]))
        if (length(nodeToRemove) > 0) xml_remove(nodeToRemove)
      }
    }
  }
  as.character(doc)
}


#' @rdname html-method
#' @exportMethod html
#' @docType methods
setMethod(
  "html", "partition",
  function(object, meta = NULL, cpos = TRUE, verbose = FALSE, cutoff = NULL, charoffset = FALSE, beautify = TRUE, ...){
    if (!requireNamespace("markdown", quietly = TRUE) && requireNamespace("htmltools", quietly = TRUE)){
      stop("package 'markdown' is not installed, but necessary for this function")
    }
    if (is.null(meta)) meta <- names(object@sAttributes)
    if (all(meta %in% sAttributes(object)) != TRUE) warning("not all sAttributes provided as meta are available")
    
    .message("generating markdown", verbose = verbose)
    md <- as.markdown(object, meta = meta, cpos = cpos, cutoff = cutoff, ...)
    
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
    md <- gsub('\u201c', '"', md)
    md <- gsub('\u201D', '"', md)
    md <- gsub('``', '"', md) # the `` would wrongly be interpreted as comments
    doc <- markdown::markdownToHTML(text = md, stylesheet = css)
    
    if (beautify) doc <- .beautify(doc)
    
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
setMethod("html", "kwic", function(object, i, sAttribute = NULL, type = NULL, verbose = FALSE){
  
  # getting metadata for all kwic lines is potentially not the fastes solution ...
  if (!is.null(sAttribute)){
    if (!sAttribute %in% sAttributes(object@corpus)) stop("sAttribute provided is not available")
    metadataDef <- sAttribute
    object <- enrich(object, meta = metadataDef)
  } else if (length(object@metadata) == 0){
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
