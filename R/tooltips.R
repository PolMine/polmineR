#' @include S4classes.R
NULL

#' Add tooltips to html document.
#' 
#' Highlight tokens based on exact match, a regular expression or corpus
#' position in kwic output or html document.
#' 
#' @param .Object a \code{html} or \code{character} object with html
#' @param tooltips a named \code{"list"} of character vectors (length 1), the 
#'   names need to match colors in the list provided to param \code{highlight}, 
#'   the value of the character vector is the tooltip to be displayed
#' @name tooltips
#' @rdname tooltips
#' @exportMethod tooltips
#' @examples
#' use("polmineR")
#' P <- partition("REUTERS", places = "argentina")
#' H <- html(P)
#' Y <- highlight(H, list(lightgreen = "higher"))
#' T <- tooltips(Y, list(lightgreen = "Further information"))
#' T
#' 
#' if (require("magrittr")){
#'   P %>%
#'     html() %>%
#'     highlight(list(yellow = c("barrels", "oil", "gas"))) %>%
#'     tooltips(list(yellow = "energy"))
#' }
setGeneric("tooltips", function(.Object, tooltips) standardGeneric("tooltips"))



#' @rdname tooltips
setMethod("tooltips", "character", function(.Object, tooltips = list()){
  if (!requireNamespace("xml2", quietly = TRUE)) stop("package 'xml2' required but not installed")
  doc <- xml2::read_html(.Object)
  for (color in names(tooltips)){
    nodes <- xml2::xml_find_all(doc, xpath = sprintf('//span[@style="background-color:%s"]', color))
    lapply(
      nodes,
      function(node){
        # turn node to a child of itself
        xml2::xml_add_child(.x = node, .value = node, copy = TRUE)
        
        # make div for a tooltip
        xml2::xml_text(node) <- ""
        xml2::xml_name(node) <- "span"
        xml2::xml_attrs(node) <- c(class = "tooltip")
        
        # create actual tooltip node (dummy first)
        children <- xml2::xml_children(node)
        xml2::xml_add_child(.x = node, .value = children[[1]], copy = TRUE)
        
        # get tooltip node and bring it in proper shape
        children <- xml2::xml_children(node)
        tipnode <- children[[length(children)]]
        xml2::xml_text(tipnode) <- tooltips[[color]]
        xml2::xml_name(tipnode) <- "span"
        xml2::xml_attrs(tipnode) <- c(class = "tooltiptext")
        
        return( NULL )
      }
    )
  }
  as.character(doc)
})

#' @rdname tooltips
setMethod("tooltips", "html", function(.Object, tooltips = list()){
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("package 'htmltools' required but not available")
  htmltools::HTML(tooltips(as.character(.Object), tooltips = tooltips))
})
