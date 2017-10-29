#' Highlight tokens.
#' 
#' Highlight tokens based on exact match, a regular expression or corpus
#' position in kwic output or html document.
#' 
#' @param .Object a \code{html} or \code{character} object with html
#' @param tooltips a named \code{"list"} of character vectors (length 1), the 
#'   names need to match colors in the list provided to param \code{highlight}, 
#'   the value of the character vector is the tooltip to be displayed
#' @param regex logical, whether character vectors give regular expressions
#' @param perl logical, whether to use perl-style regular expressions for
#'   highlighting when regex is TRUE
#' @param ... further parameters (unused)
#' @name highlight
#' @rdname highlight
#' @exportMethod highlight
#' @examples
#' use("polmineR")
#' P <- partition("REUTERS", places = "argentina")
#' H <- html(P)
#' Y <- highlight(H, list(lightgreen = "higher"), tooltips = list(lightgreen = "green!"))
#' htmltools::html_print(Y)
#' 
#' # highlight matches for a CQP query
#' highlight(
#'   H,
#'   highlight = list(yellow = cpos(hits(P, query = '"\\d+\\.*\\d*"', cqp = TRUE)))
#' )
#' 
#' # the method can be used in pipe
#' if (require("magrittr")){
#'   P %>% html() %>% highlight(list(lightgreen = "1986"))
#'   P %>% html() %>% highlight(list(lightgreen = c("1986", "higher")))
#'   P %>% html() %>% highlight(list(lightgreen = 4020:4023))
#'   P %>% html() %>% highlight(
#'     list(lightgreen = 4020:4023),
#'     tooltips = list(lightgreen = "foo")
#'     )
#'   
#' }
#' 
#' K <- kwic("REUTERS", query = "barrel")
#' K2 <- highlight(K, highlight = list(yellow = c("oil", "price")))
#' K2
setGeneric("highlight", function(.Object, ...) standardGeneric("highlight"))



#' @rdname highlight
setMethod("highlight", "character", function(.Object, highlight = list(), tooltips = NULL){
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
            if (color %in% names(tooltips)){
              # turn node to a child of itself
              xml2::xml_add_child(.x = node, .value = node, copy = TRUE)
              
              # make div for a tooltip
              xml2::xml_text(node) <- ""
              xml2::xml_name(node) <- "span"
              xml2::xml_attrs(node) <- c(class = "tooltip")
              
              # create actual tooltip node
              children <- xml2::xml_children(node)
              xml2::xml_add_child(.x = node, .value = children[[1]], copy = TRUE)
              children <- xml2::xml_children(node)
              tipnode <- children[[length(children)]]
              xml2::xml_text(tipnode) <- tooltips[[color]]
              xml2::xml_name(tipnode) <- "span"
              xml2::xml_attrs(tipnode) <- c(class = "tooltiptext")
              
            }
            NULL
          }
        )
      }
    )
  }
  as.character(doc)
})

#' @rdname highlight
setMethod("highlight", "html", function(.Object, highlight = list(), tooltips = NULL){
  htmltools::HTML(
    highlight(as.character(.Object), highlight = highlight, tooltips = tooltips)
  )
})

#' @rdname kwic-class
setMethod("highlight", "kwic", function(.Object, highlight = list(), regex = FALSE, perl = TRUE, tooltips = NULL, verbose = TRUE){
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