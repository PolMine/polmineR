#' Add hypertext reference to html document.
#' 
#' @param x Object of class 'html'.
#' @param href A named `list` with hypertext references that will be inserted as
#'   attribute href of a elements. The names of the list are either colors of
#'   highlighted text that has been generated previously, or corpus positions.
#' @export
#' @inheritParams tooltips-method
#' @name href-function
#' @aliases href
#' @examples
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' a <- corpus("REUTERS") %>%
#'   subset(places = "argentina") %>%
#'   html() %>%
#'   highlight(lightgreen = 3997) %>%
#'   href(href = list("3997" = "https://en.wikipedia.org/wiki/Argentina"))
#'   
#' if (interactive()) show(a)
href <- function(x, href, fmt, verbose = TRUE){
  
  if (!inherits(x, "html")) stop("x required to inherit from 'html' class.")
  
  if (!requireNamespace("xml2", quietly = TRUE))
    stop("package 'xml2' required but not installed")
  
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("package 'htmltools' required but not available")
  
  if (missing(fmt)){
    if (all(names(href) %in% colors() | grepl("#[0-9a-fA-F]{6}", names(href)))){
      if (verbose) cli_alert_info("assign href based on color")
      fmt <- '//span[@style="background-color:%s"]'
    } else if (!any(is.na(suppressWarnings(as.integer(names(href)))))){
      if (verbose) cli_alert_info("assign href based on cpos")
      fmt <- '//span[@id="%s"]'
    }
  }

  doc <- xml2::read_html(x)
  
  for (lookup in names(href)){
    
    nodes <- xml2::xml_find_all(doc, xpath = sprintf(fmt, lookup))
    
    lapply(
      nodes,
      function(node){
        # turn node to a child of itself
        xml2::xml_add_child(.x = node, .value = node, copy = TRUE)
        
        # turn node into link
        xml2::xml_text(node) <- ""
        xml2::xml_name(node) <- "a"
        xml2::xml_attrs(node) <- c(href = href[[lookup]])
      }
    )
  }
  
  ret <- htmltools::HTML(as.character(doc))
  attr(ret, "browsable_html") <- TRUE
  ret
}
