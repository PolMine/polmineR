#' Get and set templates.
#' 
#' Templates are used to format the markdown/html output of partitions. Upon loading 
#' the polmineR package, templates for corpora are loaded into the option 'polmineR.templates'.
#' 
#' @param .Object object 
#' @param template template to assign (a list)
#' @param verbose logical, whether to be verbose
#' @param ... further parameters
#' @exportMethod getTemplate
#' @exportMethod setTemplate
#' @rdname templates
setGeneric("getTemplate", function(.Object, ...) standardGeneric("getTemplate"))

#' @rdname templates
setMethod("getTemplate", "character", function(.Object){
  if (.Object %in% names(getOption("polmineR.templates"))){
    return(getOption("polmineR.templates")[[.Object]])
  } else {
    warning("the template requested is not available")
    return(NULL)
  }
})

#' @rdname templates
setMethod("getTemplate", "partition", function(.Object){
  getOption("polmineR.templates")[[.Object@corpus]]
})

#' @rdname templates
setMethod("getTemplate", "missing", function(.Object){
  return( names(getOption("polmineR.templates")) ) 
})

#' @rdname templates
setGeneric("setTemplate", function(.Object, ... ) standardGeneric("setTemplate"))

#' @rdname templates
setMethod("setTemplate", "character", function(.Object, template){
  templateList <- getOption("polmineR.templates")
  templateList[[.Object]] <- template
  options("polmineR.templates" = templateList)
})

#' @rdname templates
setMethod("setTemplate", "missing", function(.Object, verbose = FALSE){
  if (length(Sys.getenv("CORPUS_REGISTRY")) > 0){
    for (.Object in grep("PLPR", corpus()[["corpus"]], value = TRUE)){
      if (verbose) message("template plpr for ", .Object)
      setTemplate(.Object, getTemplate("plpr"))
    }
    for (C in corpus()[["corpus"]]){
      filename <- file.path(RegistryFile$new(C)$getHome(), "template.R")
      if (file.exists(filename)){
        if (verbose) message("customized template found for ", C)
        setTemplate(C, eval(parse(filename)))
      }
    }
  }
})