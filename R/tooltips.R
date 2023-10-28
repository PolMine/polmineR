#' @include S4classes.R
NULL

#' Add tooltips to text output.
#' 
#' Highlight tokens based on exact match, a regular expression or corpus
#' position in `kwic` output or html document.
#' 
#' @param .Object A `html` or `character` object with html.
#' @param tooltips A named `list` of character vectors, the names need to
#'   match colors in the list provided to param `highlight`. The value of
#'   the character vector is the tooltip to be displayed.
#' @param regex A `logical` value, whether character vector values of argument
#'   `tooltips` are interpreted as regular expressions.
#' @param fmt A format string with an xpath expression used to look up the node
#'   where the tooltip is inserted. If missing, a heuristic evaluating the names
#'   of the `tooltips` list decides whether tooltips are inserted based on 
#'   highlighting colors or corpus positions.
#' @param verbose A `logical` value, whether to show messages.
#' @param ... Further arguments are interpreted as assignments of tooltips to
#'   tokens.
#' @name tooltips-method
#' @aliases tooltips
#' @rdname tooltips
#' @exportMethod tooltips
#' @importFrom grDevices colors
#' @examples
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' a <- partition("REUTERS", places = "argentina")
#' b <- html(a)
#' c <- highlight(b, lightgreen = "higher")
#' d <- tooltips(c, list(lightgreen = "Further information"))
#' if (interactive()) d
#' 
#' # Using the tooltips-method in a pipe ...
#' h <- a %>%
#'   html() %>%
#'   highlight(yellow = c("barrels", "oil", "gas")) %>%
#'   tooltips(list(yellow = "energy"))
setGeneric("tooltips", function(.Object, tooltips, ...)
  standardGeneric("tooltips")
)
  



#' @rdname tooltips
setMethod("tooltips", "character", function(.Object, tooltips = list(), fmt = '//span[@style="background-color:%s"]', verbose){
  
  if (!requireNamespace("xml2", quietly = TRUE))
    stop("package 'xml2' required but not installed")
  
  doc <- xml2::read_html(.Object)
  for (lookup in names(tooltips)){
    nodes <- xml2::xml_find_all(
      doc,
      xpath = sprintf(fmt = fmt, lookup)
    )
    lapply(
      nodes,
      function(node){
        # turn node to a child of itself
        xml2::xml_add_child(.x = node, .value = node, copy = TRUE)
        
        # make div for a tooltip
        xml2::xml_text(node) <- ""
        xml2::xml_name(node) <- "span"
        xml2::xml_attrs(node) <- c(class = "tooltipping")
        
        # create actual tooltip node (dummy first)
        children <- xml2::xml_children(node)
        xml2::xml_add_child(.x = node, .value = children[[1]], copy = TRUE)
        
        # get tooltip node and bring it in proper shape
        children <- xml2::xml_children(node)
        tipnode <- children[[length(children)]]
        xml2::xml_text(tipnode) <- tooltips[[lookup]]
        xml2::xml_name(tipnode) <- "span"
        xml2::xml_attrs(tipnode) <- c(class = "tooltippingtext")
        
        return( NULL )
      }
    )
  }
  as.character(doc)
})

#' @rdname tooltips
setMethod("tooltips", "html", function(.Object, tooltips = list(), fmt, verbose = TRUE){
  
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("package 'htmltools' required but not available")
  
  if (missing(fmt)){
    if (all(names(tooltips) %in% colors() | grepl("#[0-9a-fA-F]{6}", names(tooltips)))){
      if (verbose) cli_alert_info("assign tooltips based on color")
      fmt <- '//span[@style="background-color:%s"]'
    } else if (!any(is.na(suppressWarnings(as.integer(names(tooltips)))))){
      if (verbose) cli_alert_info("assign tooltips based on cpos")
      fmt <- '//span[@id="%s"]'
    }
  }
  
  ret <- htmltools::HTML(
    tooltips(
      .Object = as.character(.Object),
      tooltips = tooltips,
      fmt = fmt,
      verbose = verbose
    )
  )
  attr(ret, "browsable_html") <- TRUE
  ret
})


#' @rdname tooltips
setMethod("tooltips", "kwic", function(.Object, tooltips, regex = FALSE, ...){
  
  if (!requireNamespace("htmltools", quietly = TRUE)){
    stop("package 'htmltools' required but not available")
  }
  
  if (length(list(...)) > 0L) tooltips <- list(...)
  
  # If argument tooltips is a list, turn it into named character vector,
  # the names of the list will then be the names of the vector.
  if (is.list(tooltips)) tooltips <- do.call(
    c,
    lapply(
      1L:length(tooltips),
      function(i)
        setNames(
          tooltips[[i]],
          rep(names(tooltips)[[i]], times = length(tooltips[[i]]))
        )
      )
  )
  
  if (regex){
    pb <- txtProgressBar(min = 0L, max = length(tooltips))
    for (i in 1L:length(tooltips)){
      .Object@cpos[["word"]] <- ifelse(
        grepl(
          sprintf("^(<.*?>|)%s(<.*?>|)$", unname(tooltips[[i]])),
          .Object@cpos[["word"]]
        ),
        sprintf(
          '<span class="tooltipping">%s<span class="tooltippingtext">%s</span></span>',
          .Object@cpos[["word"]], names(tooltips)[[i]]
        ),
        .Object@cpos[["word"]]
      )
      setTxtProgressBar(pb, value = i)
    }
    close(pb)
  } else {
    tips_rev <- setNames(object = names(tooltips), nm = unname(tooltips))
    if (length(names(tips_rev)) > length(unique(names(tips_rev)))){
      tips_rev <- sapply(
        split(x = unname(tips_rev), f = names(tips_rev)),
        `[[`, 1
      )
    }
    words <- gsub(
      "^(<.*?>|)(.*?)(<.*?>|)$", "\\2",
      .Object@cpos[["word"]], perl = TRUE
    )
    tips_vec <- tips_rev[words]
    
    .Object@cpos[["word"]] <- ifelse(
      is.na(tips_vec),
      .Object@cpos[["word"]],
      sprintf(
        '<span class="tooltipping">%s<span class="tooltippingtext">%s</span></span>',
        .Object@cpos[["word"]], tips_vec
      )
    )
  }
  enrich(.Object, table = TRUE, s_attributes = .Object@metadata)
})
