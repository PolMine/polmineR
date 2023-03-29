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
#' @param ... Further arguments are interpreted as assignments of tooltips to
#'   tokens.
#' @name tooltips
#' @rdname tooltips
#' @exportMethod tooltips
#' @examples
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' P <- partition("REUTERS", places = "argentina")
#' H <- html(P)
#' Y <- highlight(H, lightgreen = "higher")
#' T <- tooltips(Y, list(lightgreen = "Further information"))
#' if (interactive()) T
#' 
#' # Using the tooltips-method in a pipe ...
#' h <- P %>%
#'   html() %>%
#'   highlight(yellow = c("barrels", "oil", "gas")) %>%
#'   tooltips(list(yellow = "energy"))
setGeneric("tooltips", function(.Object, tooltips, ...){
  standardGeneric("tooltips")
})
  



#' @rdname tooltips
setMethod("tooltips", "character", function(.Object, tooltips = list()){
  
  if (!requireNamespace("xml2", quietly = TRUE)){
    stop("package 'xml2' required but not installed")
  }
  doc <- xml2::read_html(.Object)
  for (color in names(tooltips)){
    nodes <- xml2::xml_find_all(
      doc,
      xpath = sprintf('//span[@style="background-color:%s"]', color)
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
        xml2::xml_text(tipnode) <- tooltips[[color]]
        xml2::xml_name(tipnode) <- "span"
        xml2::xml_attrs(tipnode) <- c(class = "tooltippingtext")
        
        return( NULL )
      }
    )
  }
  as.character(doc)
})

#' @rdname tooltips
setMethod("tooltips", "html", function(.Object, tooltips = list()){
  
  if (!requireNamespace("htmltools", quietly = TRUE)){
    stop("package 'htmltools' required but not available")
  }
  ret <- htmltools::HTML(tooltips(as.character(.Object), tooltips = tooltips))
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
