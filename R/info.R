#' @include S4classes.R
NULL

#' @rdname polmineR-generics
setGeneric("get_info", function(x) standardGeneric("get_info"))

#' @rdname polmineR-generics
setGeneric("show_info", function(x) standardGeneric("show_info"))

#' @rdname corpus_methods
#' @details Use \code{get_info} to get the the content of the info file for the
#'   corpus (usually in the data directory of the corpus) and return it as a
#'   \code{character} vector. Returns \code{NULL} if there is not info file.
#' @export get_info
setMethod("get_info", "corpus", function(x){
  fname <- registry_get_info(corpus = x@corpus, registry = registry())
  if (file.exists(fname)){
    info <- readLines(fname)
    attr(info, "md") <- if (grepl(".md$", fname)) TRUE else FALSE
  } else {
    info <- NULL
  }
  info
})

#' @export show_info
#' @rdname corpus_methods
#' @details The \code{show_info}-method will get the content of the info
#'   file for a corpus, turn it into an html document, and show the result in
#'   the viewer pane of RStudio. If the filename of the info file ends on "md",
#'   the document is rendered as markdown.
setMethod("show_info", "corpus", function(x){
  info_plain <- get_info(x)
  if (!is.null(info_plain)){
    if (attr(info_plain, "md")){
      info_html_raw <- markdown::markdownToHTML(text = info_plain) 
      info_html <- htmltools::HTML(
        gsub("^.*<body>(.*?)</body>.*?$", "\\1", as.character(info_html_raw))
      )
    } else {
      info_html <- htmltools::HTML(info_plain)
    }
  } else {
    info_html <- htmltools::HTML("</br><i>corpus info file not found</i>")
  }
  attr(info_html, "browsable_html") <- TRUE
  if (interactive()) htmltools::html_print(info_html)
  invisible(info_html)
})