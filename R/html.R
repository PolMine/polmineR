#' @include partition.R partition_bundle.R S4classes.R
NULL

#' Generate html from object.
#' 
#' Prepare html document to see full text.
#' 
#' @return Returns an object of class \code{html} as used in the \code{htmltools} package. Methods
#' such as \code{htmltools::html_print} will be available. The encoding of the html
#' document will be UTF-8 on all systems (including Windows).
#' 
#' @details If param \code{charoffset} is \code{TRUE}, character offset positions will be
#' added to tags that embrace tokens. This may be useful, if exported html document
#' is annotated with a tool that stores annotations with character offset positions.
#' 
#' @param object The object the fulltext output will be based on.
#' @param meta Metadata to include in  output, if \code{NULL} (default), the
#'   s-attributes defining a partition will be used.
#' @param s_attribute Structural attributes that will be used to define the partition 
#' where the match occurred.
#' @param cpos Length-one \code{logical} value, if \code{TRUE} (default), all
#'   tokens will be wrapped by elements with id attribute indicating corpus
#'   positions.
#' @param corpus The ID of the corpus, a length-one \code{character} vector.
#' @param beautify Length-one \code{logical} value, if \code{TRUE}, whitespace
#'   before interpunctuation will be removed.
#' @param charoffset Length-one \code{logical} value, if \code{TRUE}, character
#'   offset positions are added to elements embracing tokens.
#' @param height A \code{character} vector that will be inserted into the html
#'   as an optional height of a scroll box.
#' @param verbose Length-one \code{logical} value, whether to output progress
#'   messages.
#' @param progress Length-one \code{logical} value, whether to output progress#
#'   bar.
#' @param cutoff An \code{integer} value, maximum number of tokens to decode
#'   from token stream, passed into \code{as.markdown}.
#' @param type The partition type.
#' @param i An \code{integer} value: If \code{object} is a \code{kwic}-object,
#'   the index of the concordance for which the fulltext is to be generated.
#' @param ... Further parameters that are passed into \code{as.markdown}.
#' @rdname html-method
#' @aliases show,html-method
#' @examples
#' use("polmineR")
#' P <- partition("REUTERS", places = "argentina")
#' H <- html(P)
#' if (interactive()) H # show full text in viewer pane
#' 
#' # html-method can be used in a pipe
#' H <- partition("REUTERS", places = "argentina") %>% html()
#'   
#' # use html-method to get full text where concordance occurrs
#' K <- kwic("REUTERS", query = "barrels")
#' H <- html(K, i = 1, s_attribute = "id")
#' H <- html(K, i = 2, s_attribute = "id")
#' for (i in 1L:length(K)) {
#'   H <- html(K, i = i, s_attribute = "id")
#'   if (interactive()){
#'     show(H)
#'     userinput <- readline("press 'q' to quit or any other key to continue")
#'     if (userinput == "q") break
#'   }
#' }
#'   
setGeneric("html", function(object, ...) standardGeneric("html") )


#' @exportMethod html
#' @rdname html-method
setMethod("html", "character", function(object, corpus, height = NULL){
  if (!requireNamespace("markdown", quietly = TRUE))
    stop("package 'markdown' is not installed, but necessary for this function")

  css <- paste(c(
    readLines(getOption("markdown.HTML.stylesheet")),
    readLines(system.file("css", "tooltips.css", package = "polmineR"))
  ), collapse = "\n", sep = "\n"
  )
  md <- gsub('\u201c', '"', object)
  md <- gsub('\u201D', '"', md)
  md <- gsub('``', '"', md) # the `` would wrongly be interpreted as comments

  # produce result very similar to markdown::markdownToHTML, but selfmade to
  # circumvent encoding issue on windows (poor handling of encodings other
  # than UTF-8 by markdownToHTML)
  template <- "<!DOCTYPE html>\n<html>\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n<title>%s</title>\n<style type=\"text/css\">\n%s\n</style>\n</head>\n<body>\n%s\n</body>\n</html>"
  doc <- sprintf(
    template,
    sprintf("Corpus: %s", corpus), # title
    css,
    markdown::renderMarkdown(text = md)
  )

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
  doc
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
          1L:length(nodes),
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
        node_to_remove <- xml_find_first(doc, xpath = sprintf('//span[@id = "%d"]', cpos[i]))
        if (length(node_to_remove) > 0) xml_remove(node_to_remove)
      }
    }
  }
  as.character(doc)
}


#' @rdname html-method
setMethod(
  "html", "partition",
  function(object, meta = NULL, cpos = TRUE, verbose = FALSE, cutoff = NULL, charoffset = FALSE, beautify = TRUE, height = NULL, ...){
    newobj <- if (is.null(get_type(object))){
      "subcorpus"
    } else {
      switch( get_type(object), "plpr" = "plpr_subcorpus", "press" = "press_subcorpus" )
    }
    
    html(
      object = as(object, newobj),
      meta = meta,
      cpos = cpos,
      verbose = verbose,
      cutoff = cutoff,
      charoffset = charoffset,
      beautify = beautify,
      height = height,
      ...
    )
})


#' @rdname html-method
#' @exportMethod html
#' @docType methods
setMethod(
  "html", "subcorpus",
  function(object, meta = NULL, cpos = TRUE, verbose = FALSE, cutoff = NULL, charoffset = FALSE, beautify = FALSE, height = NULL, ...){
    if (!requireNamespace("markdown", quietly = TRUE) && requireNamespace("htmltools", quietly = TRUE)){
      stop("package 'markdown' is not installed, but necessary for this function")
    }
    if (is.null(meta)) meta <- names(object@s_attributes)
    if (isFALSE(all(meta %in% s_attributes(object)))) warning("not all s-attributes provided as meta are available")
    
    .message("generating markdown", verbose = verbose)
    md <- as.markdown(object, meta = meta, cpos = cpos, cutoff = cutoff, ...)
    
    .message("markdown to html", verbose = verbose)
    # the css for tooltips is included in the html document by default,
    # even if this causes some (minimal) overhead.
    # The default stylesheet (markdown.css) needs to be included explicitly,
    # so that it is not lost.
    
    doc <- html(object = md, corpus = object@corpus, height = height)
    if (beautify) doc <- .beautify(doc)
    if (charoffset) doc <- .addCharacterOffset(doc)
    ret <- htmltools::HTML(doc)
    attr(ret, "browsable_html") <- TRUE
    ret
  }
)

#' @docType methods
#' @rdname html-method
setMethod("html", "partition_bundle", function(object, charoffset = FALSE, beautify = TRUE, height = NULL, progress = TRUE, ...){
  
  corpus_id <- get_corpus(object)
  
  if (!requireNamespace("markdown", quietly = TRUE))
    stop("package 'markdown' is not installed, but necessary for this function")
  
  md_list <- if (isTRUE(progress)){
    pblapply(object@objects, function(p) as.markdown(p, ...))
  } else {
    lapply(object@objects, function(p) as.markdown(p, ...))
  }
  
  md <- paste(md_list, collapse = "\n* * *\n")
  md <- paste(
    paste('## Excerpt from corpus', corpus_id, '\n* * *\n'),
    md,
    '\n* * *\n',
    collapse = "\n"
  )

  doc <- html(object = md, corpus = corpus_id, height = height)
  if (beautify) doc <- .beautify(doc)
  if (charoffset) doc <- .addCharacterOffset(doc)
  ret <- htmltools::HTML(doc)
  attr(ret, "browsable_html") <- TRUE
  ret
  
  
})


#' @rdname html-method
setMethod("html", "kwic", function(object, i, s_attribute = NULL, type = NULL, verbose = FALSE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  # getting metadata for all kwic lines is potentially not the fastes solution ...
  if (!is.null(s_attribute)){
    if (!s_attribute %in% s_attributes(object@corpus)) stop("s-attribute provided is not available")
    s_attrs <- s_attribute
    object <- enrich(object, s_attributes = s_attrs)
  } else if (length(object@metadata) == 0L){
    s_attrs <- get_template(object@corpus)[["metadata"]]
    .message("using metadata from template: ", paste(s_attrs, collapse = " / "), verbose = verbose)
    if (length(s_attrs) > 0L){
      .message("enriching", verbose = verbose)
      object <- enrich(object, meta = s_attrs)
    }
  } else {
    s_attrs <- object@metadata
  }
  
  partition_to_read <- partition(
    object@corpus,
    def = lapply(setNames(s_attrs, s_attrs), function(x) object@stat[[x]][i]),
    type = type
  )
  .message("generating html", verbose = verbose)
  fulltext <- polmineR::html(partition_to_read, meta = s_attrs, cpos = TRUE)
  .message("generating highlights", verbose = verbose)
  tabSubset <- object@cpos[which(object@cpos[["match_id"]] == i)]
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

