#' @include S4classes.R
NULL


#' Highlight tokens.
#' 
#' Highlight tokens based on exact match, a regular expression or corpus
#' position in kwic output or html document.
#' 
#' @param .Object a \code{html} or \code{character} object with html, or a 
#'   \code{kwic} object
#' @param highlight a \code{"list"} of character or integer vectors, the names 
#'   need to provide the colors, the values of the vector the term to be matched
#'   or a corpus position
#' @param regex logical, whether character vectors give regular expressions
#' @param perl logical, whether to use perl-style regular expressions for
#'   highlighting when regex is TRUE
#' @param verbose logical, whether to output verbose messages
#' @param ... further parameters (unused)
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
setGeneric("highlight", function(.Object, ...) standardGeneric("highlight"))



#' @rdname highlight
setMethod("highlight", "character", function(.Object, highlight = list()){
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
setMethod("highlight", "html", function(.Object, highlight = list()){
  htmltools::HTML(
    highlight(as.character(.Object), highlight = highlight)
  )
})

#' @rdname highlight
setMethod("highlight", "kwic", function(.Object, highlight = list(), regex = FALSE, perl = TRUE, verbose = TRUE){
  for (color in names(highlight)){
    if (regex){
      regexMatchList <- lapply(
        highlight[[color]],
        function(expr) grep(expr, .Object@cpos[["word"]], perl = perl)
      )
      toHighlight <- 1:nrow(.Object@cpos) %in% unique(unlist(regexMatchList))
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
  .Object
})