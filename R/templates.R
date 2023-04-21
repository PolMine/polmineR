#' @include S4classes.R
NULL

#' Get template for formatting full text output.
#' 
#' Templates are used to format the markdown/html of fulltext. `get_template()`
#' loads and parses the content of the JSON template file, if it is found in the
#' slot "template" of `.Object`.
#' 
#' To learn how to write templates, consult the sample files in the folder
#' "templates" of the installed package - see examples.
#' @details 
#' @param .Object A `corpus`, `subcorpus` or `partition` object,
#'   or a length-one `character` vector with a corpus ID.
#' @param warn A `logical` value, whether to issue a warning if template 
#'   is not available. Defaults to `FALSE`.
#' @param ... Further arguments.
#' @return If a template is available, a `list` with the parsed content of the
#'   template file, otherwise `NULL`.
#' @exportMethod get_template
#' @rdname templates
#' @examples
#' use("polmineR")
#' corpus("GERMAPARLMINI") %>%
#'   get_template()
#'
#' # template files included in the package   
#' template_dir <- system.file(package = "polmineR", "templates")
#' list.files(template_dir)
setGeneric(
  "get_template",
  function(.Object, ...) standardGeneric("get_template")
)

#' @rdname templates
setMethod("get_template", "character", function(.Object, warn = FALSE){
  get_template(corpus(.Object), warn = warn)
})

#' @rdname templates
setMethod("get_template", "corpus", function(.Object, warn = FALSE){
  if (is.na(.Object@template)){
    if (warn) cli_alert_warning(
      "No template available for corpus {.val {.Object@corpus}}."
    )
    return(NULL)
  } else {
    y <- jsonlite::fromJSON(txt = .Object@template) 
    if ("metadata" %in% names(y)) y[["metadata"]] <- unlist(y[["metadata"]])
    return(y)
  }
})


#' @rdname templates
setMethod("get_template", "subcorpus", function(.Object, warn = FALSE){
  callNextMethod()
})
