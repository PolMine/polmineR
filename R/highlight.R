#' @include S4classes.R
NULL


#' Highlight tokens in text output.
#'
#' Highlight tokens in fulltext based on exact match, a regular expression or
#' corpus position in `kwic` output or `html` document.
#'
#' If `highlight` is a character vector, the names of the vector are interpreted
#' as colors. If `highlight` is a list, the names of the list are considered as
#' colors. Values can be character values or integer values with token ids.
#' Colors are inserted into the output html and need to be digestable for the
#' browser used.
#' 
#' @param .Object A `html`, `character`, a `kwic` object.
#' @param highlight A character vector, or a list of `character` or `integer`
#'   vectors.
#' @param regex Logical, whether character vectors are interpreted as regular
#'   expressions.
#' @param perl Logical, whether to use perl-style regular expressions for
#'   highlighting when regex is `TRUE`.
#' @param verbose Logical, whether to output messages.
#' @param ... Terms to be highlighted can be passed in as named `character`
#'   vectors of terms (or regular expressions); the name needs to be a
#'   valid color name. It is also possible to pass in a `matrix` with ranges (as
#'   returned by `cpos()`).
#' @name highlight-method
#' @aliases highlight
#' @rdname highlight
#' @exportMethod highlight
#' @examples
#' use("polmineR")
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' P <- partition("REUTERS", places = "argentina")
#' H <- html(P)
#' Y <- highlight(H, list(lightgreen = "higher"))
#' if (interactive()) htmltools::html_print(Y)
#' 
#' # highlight matches for a CQP query
#' regions <- cpos(P, query = '"prod.*"', cqp = TRUE)
#' H2 <- highlight(H, highlight = list(yellow = regions))
#' 
#' # the method can be used in pipe
#' P %>% html() %>% highlight(list(lightgreen = "1986")) -> H
#' P %>% html() %>% highlight(list(lightgreen = c("1986", "higher"))) -> H
#' P %>% html() %>% highlight(list(lightgreen = 4020:4023)) -> H
#' 
#' # use highlight for kwic output
#' K <- kwic("REUTERS", query = "barrel")
#' K2 <- highlight(K, highlight = list(yellow = c("oil", "price")))
#' 
#' # use character vector for output, not list
#' K2 <- highlight(
#'   K,
#'   highlight = c(
#'     green = "pric.",
#'     red = "reduction",
#'     red = "decrease",
#'     orange = "dropped"
#'   ),
#'   regex = TRUE
#' )
setGeneric("highlight", function(.Object, ...) standardGeneric("highlight"))



#' @rdname highlight
setMethod("highlight", "character", function(.Object, highlight = list(), regex = FALSE, perl = FALSE, ...){
  if (length(list(...)) > 0) highlight <- list(...)
  
  if (is.character(highlight))
    highlight <- split(x = unname(highlight), f = names(highlight))
  
  if (!requireNamespace("xml2", quietly = TRUE))
    stop("package 'xml2' needs to be installed for highlighting using cpos/ids")
  doc <- xml2::read_html(.Object)
  for (color in names(highlight)){
    lapply(
      highlight[[color]],
      function(x){
        if (is.numeric(x)){
          nodes <- xml2::xml_find_all(
            doc,
            xpath = sprintf('//span[@id="%d"]', x)
          )
        } else {
          if (!regex){
            nodes <- xml2::xml_find_all(
              doc, xpath = sprintf('//span[@token="%s"]', x)
            )
          } else {
            span_nodes <- xml2::xml_find_all(
              doc, xpath = sprintf('//span', x)
            )
            token_attrs <- lapply(span_nodes, function(x) xml_attrs(x, "token"))
            nodes <- span_nodes[
              grep(x, sapply(token_attrs, function(x) x[["token"]]), perl = perl)
            ]
          }
        }
        lapply(
          nodes,
          function(node){
            xml2::xml_set_attr(
              node,
              attr = "style",
              value = sprintf("background-color:%s", color)
            )
            NULL
          }
        )
      }
    )
  }
  as.character(doc)
})

#' @rdname highlight
setMethod("highlight", "html", function(.Object, highlight = list(), regex = FALSE, perl = FALSE, ...){
  if (length(list(...)) > 0L) highlight <- list(...)
  ret <- htmltools::HTML(
    highlight(
      as.character(.Object),
      highlight = highlight,
      regex = regex,
      perl = perl
    )
  )
  attr(ret, "browsable_html") <- TRUE
  ret
})

#' @rdname highlight
setMethod("highlight", "kwic", function(.Object, highlight = list(), regex = FALSE, perl = TRUE, verbose = TRUE, ...){
  if (length(list(...)) > 0L) highlight <- list(...)
  
  if (is.character(highlight))
    highlight <- split(x = unname(highlight), f = names(highlight))
  
  for (color in names(highlight)){
    if (is.matrix(highlight[[color]])){
      to_highlight <- .Object@cpos[["cpos"]] %in% cpos(highlight[[color]])
    } else {
      if (regex){
        regex_match_list <- lapply(
          highlight[[color]],
          function(expr) 
            grep(expr, .Object@cpos[[.Object@p_attribute]], perl = perl)
        )
        to_highlight <- 1L:nrow(.Object@cpos) %in% unique(unlist(regex_match_list))
      } else {
        to_highlight <- .Object@cpos[[.Object@p_attribute]] %in% highlight[[color]]
      }
    }
    
    if (length(to_highlight) > 0){
      .Object@cpos[, (.Object@p_attribute) := ifelse(
        to_highlight,
        sprintf(
          '<span style="background-color:%s">%s</span>',
          color,
          .Object@cpos[[.Object@p_attribute]]
        ),
        .Object@cpos[[.Object@p_attribute]]
      )]
    }
  }
  .Object <- enrich(.Object, table = TRUE)
  .Object <- enrich(.Object, s_attributes = unique(.Object@metadata))
  .Object
})