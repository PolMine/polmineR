#' @include kwic_class.R kwic_method.R
NULL

#' @rdname kwic-class
#' @docType method
#' @importFrom DT datatable formatStyle
#' @importFrom magrittr %>%
setMethod("show", "kwic", function(object){
  kwicLineView <- slot(get("session", ".GlobalEnv"), "kwicLineView")
  if (kwicLineView == FALSE){
    retval <- datatable(object@table) %>%
      formatStyle("node", color="blue", textAlign="center") %>%
      formatStyle("left", textAlign="right")
  } else {
    object@table[["node"]] <- paste('<span style="color:steelblue">', object@table[["node"]], '</span>', sep="")
    object@table[["text"]] <- apply(object@table, 1, function(x) paste(x[c("left", "node", "right")], collapse=" "))
    for (x in c("left", "node", "right")) object@table[[x]] <- NULL
    retval <- datatable(object@table, escape=FALSE)
  }
  show(retval)
})


#' @docType methods
#' @noRd
setMethod('[', 'kwic',
          function(x,i) {
            x@table <- x@table[i,]
            x
          }        
)

#' @rdname kwic-class
setMethod("as.data.frame", "kwic", function(x){
  metaColumnsNo <- length(colnames(x@table)) - 3
  metadata <- apply(x@table, 1, function(row) paste(row[c(1:metaColumnsNo)], collapse="<br/>"))
  data.frame(
    meta=metadata,
    leftContext=x@table$leftContext,
    node=x@table$node,
    rightContext=x@table$rightContext
  )
})

