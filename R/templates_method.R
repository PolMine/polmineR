.templates <- list(
  
  plain = list(
    document = list(
      sAttribute = "text",
      format = c("### ", "")
      )
  ),
  
  article = list(
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
)



setGeneric("templates", function(.Object, ...) standardGeneric("templates"))

setMethod("templates", "character", function(.Object){
  if (.Object %in% names(.templates)){
    return(.templates[[.Object]])
  } else {
    stop("the template requested is not available")
  }
})

setMethod("templates", "missing", function() names(.templates))
