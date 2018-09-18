#' @include S4classes.R
NULL


#' Highlight tokens in text output.
#'
#' Highlight tokens in fulltext based on exact match, a regular expression or
#' corpus position in \code{kwic} output or \code{html} document.
#'
#' If \code{highlight} is a character vector, the names of the vector are
#' interpreted as colors. If \code{highlight} is a list, the names of the list
#' are considered as colors. Values can be character values or integer values
#' with token ids. Colors are inserted into the output html and need to be
#' digestable for the browser used.
#' 
#' @param .Object A \code{html}, \code{character}, a \code{kwic} object.
#' @param highlight A character vector, or a list of character or integer vectors.
#' @param regex Logical, whether character vectors are interpreted as regular
#'   expressions.
#' @param perl Logical, whether to use perl-style regular expressions for
#'   highlighting when regex is \code{TRUE}.
#' @param verbose Logical, whether to output messages.
#' @param ... Terms to be highlighted can be passed in as named character
#'   vectors of terms (or regular expressions); the name then needs to be a
#'   valid color name.
#' @name highlight
#' @rdname highlight
#' @exportMethod highlight
#' @examples
#' use("polmineR")
#' P <- partition("REUTERS", places = "argentina")
#' H <- html(P)
#' Y <- highlight(H, list(lightgreen = "higher"))
#' if (interactive()) htmltools::html_print(Y)
#' 
#' # highlight matches for a CQP query
#' H2 <- highlight(
#'   H,
#'   highlight = list(yellow = cpos(hits(P, query = '"prod.*"', cqp = TRUE)))
#' )
#' 
#' # the method can be used in pipe
#' if (require("magrittr")){
#'   P %>% html() %>% highlight(list(lightgreen = "1986")) -> H
#'   P %>% html() %>% highlight(list(lightgreen = c("1986", "higher"))) -> H
#'   P %>% html() %>% highlight(list(lightgreen = 4020:4023)) -> H
#' }
#' 
#' # use highlight for kwic output
#' K <- kwic("REUTERS", query = "barrel")
#' K2 <- highlight(K, highlight = list(yellow = c("oil", "price")))
#' if (interactive()) K2
#' 
#' # use character vector for output, not list
#' K2 <- highlight(
#'   K,
#'   highlight = c(
#'     green = "pric.",
#'     red = "reduction",
#'     red = "decrease",
#'     orange = "dropped"),
#'     regex = TRUE
#' )
#' if (interactive()) K2
setGeneric("highlight", function(.Object, ...) standardGeneric("highlight"))



#' @rdname highlight
setMethod("highlight", "character", function(.Object, highlight = list(), ...){
  if (length(list(...)) > 0) highlight <- list(...)
  if (is.character(highlight)) highlight <- split(x = unname(highlight), f = names(highlight))
  if (!requireNamespace("xml2", quietly = TRUE)) stop("package 'xml2' needs to be installed for highlighting using cpos/ids")
  doc <- xml2::read_html(.Object)
  for (color in names(highlight)){
    lapply(
      highlight[[color]],
      function(x){
        if (is.numeric(x)){
          nodes <- xml2::xml_find_all(doc, xpath = sprintf('//span[@id="%d"]', x))
        } else {
          nodes <- xml2::xml_find_all(doc, xpath = sprintf('//span[@token="%s"]', x))
        }
        lapply(
          nodes,
          function(node){
            xml2::xml_set_attr(node, attr = "style", value = sprintf("background-color:%s", color))
            NULL
          }
        )
      }
    )
  }
  as.character(doc)
})

#' @rdname highlight
setMethod("highlight", "html", function(.Object, highlight = list(), ...){
  if (length(list(...)) > 0) highlight <- list(...)
  htmltools::HTML(
    highlight(as.character(.Object), highlight = highlight)
  )
})

#' @rdname highlight
setMethod("highlight", "kwic", function(.Object, highlight = list(), regex = FALSE, perl = TRUE, verbose = TRUE, ...){
  if (length(list(...)) > 0L) highlight <- list(...)
  if (is.character(highlight)) highlight <- split(x = unname(highlight), f = names(highlight))
  for (color in names(highlight)){
    if (regex){
      regexMatchList <- lapply(
        highlight[[color]],
        function(expr) grep(expr, .Object@cpos[["word"]], perl = perl)
      )
      toHighlight <- 1L:nrow(.Object@cpos) %in% unique(unlist(regexMatchList))
    } else {
      toHighlight <- .Object@cpos[["word"]] %in% highlight[[color]]
    }
    if (length(toHighlight) > 0){
      .Object@cpos[["word"]] <- ifelse(
        toHighlight,
        sprintf('<span style="background-color:%s">%s</span>', color, .Object@cpos[["word"]]),
        .Object@cpos[["word"]]
      )
    }
  }
  .Object <- enrich(.Object, table = TRUE)
  .Object <- enrich(.Object, s_attributes = .Object@metadata)
  .Object
})