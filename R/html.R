#' @include partition.R partition_bundle.R S4classes.R
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
#' @param s_attribute structural attributes that will be used to define the partition 
#' where the match occurred
#' @param cpos logical, if \code{TRUE} (default), all tokens will be wrapped by 
#'   elements with id attribute indicating corpus positions
#' @param beautify logical, if \code{TRUE}, whitespace before interpunctuation
#'   will be removed
#' @param charoffset logical, if \code{TRUE}, character offset positions are
#'   added to elements embracing tokens
#' @param height A character vector that will be inserted into the html as an optional
#'   height of a scroll box.
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
#'   H <- html(K, i = 1, s_attribute = "id")
#'   H <- html(K, i = 2, s_attribute = "id")
#'   for (i in 1:length(K)) {
#'     H <- html(K, i = i, s_attribute = "id")
#'     if (interactive()){
#'       show(H)
#'       userinput <- readline("press 'q' to quit or any other key to continue")
#'       if (userinput == "q") break
#'     }
#'   }
#' }
#' 
setGeneric("html", function(object, ...) standardGeneric("html") )


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
  offset <- c(0L, cumsum(nchar_textnodes)[1L:(length(nchar_textnodes) - 1L)])
  dummy <- lapply(
    1L:length(textnodes),
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
    for (i in 2L:length(cpos)){
      if (cpos[i - 1L] + 1L == cpos[i]){
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
  function(object, meta = NULL, cpos = TRUE, verbose = FALSE, cutoff = NULL, charoffset = FALSE, beautify = TRUE, height = NULL, ...){
    if (!requireNamespace("markdown", quietly = TRUE) && requireNamespace("htmltools", quietly = TRUE)){
      stop("package 'markdown' is not installed, but necessary for this function")
    }
    if (is.null(meta)) meta <- names(object@s_attributes)
    if (all(meta %in% s_attributes(object)) != TRUE) warning("not all s-attributes provided as meta are available")
    
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
    
    if (!is.null(height)){
      fmt <- '%s<div style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height: %s;">%s</div></body></html>'
      doc <- sprintf(
        fmt,
        gsub("^(.*?)<body>\n.*?</body>.*?$", "\\1", doc),
        height,
        gsub("^.*?<body>\n(.*?)</body>.*?$", "\\1", doc)
      )
      doc <- gsub("<h3>", '<h3 class="fulltext">', doc)
    }
    
    if (beautify) doc <- .beautify(doc)
    
    if (charoffset) doc <- .addCharacterOffset(doc)
    
    htmltools::HTML(doc)
  }
)

#' @docType methods
#' @rdname html-method
setMethod("html", "partition_bundle", function(object, filename = c(), type = "debate"){
  markdown <- paste(lapply(object@objects, function(p) as.markdown(p, type)), collapse="\n* * *\n")
  markdown <- paste(
    paste('## Excerpt from corpus', object@corpus, '\n* * *\n'),
    markdown,
    '\n* * *\n',
    collapse = "\n")
  if (is.null(filename)) {
    htmlFile <- html(markdown)
  } else {
    cat(markdown, file = filename)    
  }
  if (is.null(filename)) browseURL(htmlFile)
})


#' @rdname html-method
setMethod("html", "kwic", function(object, i, s_attribute = NULL, type = NULL, verbose = FALSE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  # getting metadata for all kwic lines is potentially not the fastes solution ...
  if (!is.null(s_attribute)){
    if (!s_attribute %in% s_attributes(object@corpus)) stop("s-attribute provided is not available")
    metadataDef <- s_attribute
    object <- enrich(object, s_attributes = metadataDef)
  } else if (length(object@metadata) == 0L){
    metadataDef <- getOption("polmineR.templates")[[object@corpus]][["metadata"]]
    .message("using metadata from template: ", paste(metadataDef, collapse = " / "), verbose = verbose)
    if (length(metadataDef) > 0L){
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


#' @include polmineR.R
NULL

setOldClass("html")


#' @export print.html
#' @rdname html-method
#' @S3method print html
print.html <- function(x, ...){
  if (requireNamespace("htmltools", quietly = TRUE)){
    if (interactive()) htmltools::html_print(x)
  } else {
    warning("package 'htmltools' needs to be installed, but is not available")
  }  
}
