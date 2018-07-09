#' @include S4classes.R
NULL

#' Get and set templates.
#' 
#' Templates are used to format the markdown/html output of partitions. Upon loading 
#' the polmineR package, templates for corpora are loaded into the option 'polmineR.templates'.
#' 
#' @param .Object object 
#' @param verbose logical, whether to be verbose
#' @param ... further parameters
#' @exportMethod get_template
#' @exportMethod set_template
#' @rdname templates
setGeneric("get_template", function(.Object, ...) standardGeneric("get_template"))

#' @rdname templates
setMethod("get_template", "character", function(.Object){
  if (.Object %in% names(getOption("polmineR.templates"))){
    return( getOption("polmineR.templates")[[.Object]] )
  } else {
    warning("the template requested is not available")
    return(NULL)
  }
})

#' @rdname templates
setMethod("get_template", "partition", function(.Object){
  getOption("polmineR.templates")[[.Object@corpus]]
})

#' @rdname templates
setMethod("get_template", "missing", function(.Object){
  return( names(getOption("polmineR.templates")) ) 
})

#' @rdname templates
setGeneric("set_template", function(.Object, ... ) standardGeneric("set_template"))

#' @rdname templates
setMethod("set_template", "character", function(.Object){
  stopifnot(.Object %in% CQI$list_corpora())
  templateList <- getOption("polmineR.templates")
  filename <- file.path(registry_get_home(.Object), "template.json")
  if (file.exists(filename)){
    templateList[[.Object]] <- jsonlite::fromJSON(txt = filename) 
    if ("metadata" %in% names(templateList[[.Object]])){
      templateList[[.Object]][["metadata"]] <- unlist(templateList[[.Object]][["metadata"]])
    }
  } else {
    templateList[[.Object]] <- jsonlite::fromJSON(
      txt = system.file(package = "polmineR", "templates", "plain.template.json")
      )
  }
  options("polmineR.templates" = templateList)
  invisible(templateList[[.Object]])
})


#' @rdname templates
#' @importFrom jsonlite fromJSON
setMethod("set_template", "missing", function(.Object, verbose = FALSE){
  if (length(Sys.getenv("CORPUS_REGISTRY")) > 0){
    for (x in CQI$list_corpora()) set_template(x)
  }
})