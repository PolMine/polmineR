#' Get and set templates.
#' 
#' Templates are used to format the markdown/html output of partitions. Upon loading 
#' the polmineR package, templates for corpora are loaded into the option 'polmineR.templates'.
#' 
#' @param x either a "character" vector, if missing, the names of available templates will be returned
#' @param template template to assign (a list)
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
    stop("the template requested is not available")
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

setMethod("setTemplate", "missing", function(.Object){
  if (length(Sys.getenv("CORPUS_REGISTRY")) > 0){
    for (.Object in grep("PLPR", corpus()[["corpus"]], value = TRUE)){
      setTemplate(.Object, getTemplate("plpr"))
    }
    for (C in corpus()[["corpus"]]){
      filename <- file.path(RegistryFile$new(.Object)$getHome(), "template.R")
      if (file.exists(filename)){
        setTemplate(C, eval(parse(filename)))
      }
    }
  }
})