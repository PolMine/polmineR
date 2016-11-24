#' @export .templates
.templates <- new.env(parent = .GlobalEnv)

.templates$plain = list(
  document = list(
    sAttribute = "text",
    format = c("### ", "")
  )
)

.templates$article <- list(
  document = list(
    sAttribute = "text",
    format = c("### ", "")
  ),
  paragraphs = list(
    sAttribute = "p_type",
    format = list(
      meta = c("### ", ""),
      title = c("## ", ""),
      teaser = c("_", "_\n"),
      body = c("", "\n"),
      highlight = c("_", "_\n"),
      headline = c("# ", "")
    )
  )
)

.templates$plpr <- list(
  metadata = c("text_speaker", "text_date", "text_party"),
  document = list(
    sAttribute = "text",
    format = c("\n### ", "\n")
  ),
  speech = list(
    sAttribute = "text_type",
    format = list(
      speech = c("", ""),
      interjection = c("\n> ", "\n")
    )
  )
)


#' Manage templates.
#' 
#' When the polmineR package is loaded, an environment .templates will be assigned to the global 
#' environment that will is used when preparing the html output for reading the text in a partition.
#' The 
#' 
#' @param x either a "character" vector, if missing, the names of available templates will be returned
#' @param template template to assign (a list)
#' @param ... further parameters
#' @rdname templates
setGeneric("getTemplate", function(x) standardGeneric("getTemplate"))

#' @rdname templates
setMethod("getTemplate", "character", function(x){
  if (x %in% names(.templates)){
    return(.templates[[x]])
  } else {
    stop("the template requested is not available")
  }
})

#' @exportMethod getTemplate
#' @rdname templates
setMethod("getTemplate", "missing", function() names(.templates))

#' @rdname templates
setGeneric("setTemplate", function(x, ...) standardGeneric("setTemplate"))

#' @export setTemplate
#' @rdname templates
setMethod("setTemplate", "character", function(x, template) {
  .templates[[x]] <- template
})

#' @rdname templates
setMethod("setTemplate", "missing", function(){
  # conditional on CORPUS_REGISTRY being set (for CRAN tests)
  if (length(Sys.getenv("CORPUS_REGISTRY")) > 0){
    for (x in grep("PLPR", corpus()[["corpus"]], value = TRUE)){
      setTemplate(x, getTemplate("plpr"))
    }
    
    for (x in corpus()[["corpus"]]){
      filename <- file.path(parseRegistry(x)[["HOME"]], "template.R")
      if (file.exists(filename)){
        source(filename)
      }
    }
  }
})