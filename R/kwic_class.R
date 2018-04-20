#' kwic (S4 class)
#' 
#' S4 class for organizing information for concordance output
#' 
#' @details The \code{enrich} method is used to generate the actual output for
#' the kwic method. If param \code{table} is \code{TRUE}, corpus positions will
#' be turned into a data.frame with the concordance lines. If param \code{meta}
#' is a character vector with s-attributes, the respective s-attributes will be
#' added as columns to the table with concordance lines.
#' 
#' @slot metadata Object of class \code{"character"} keeping the sAttributes of the metadata that are to be displayed
#' @slot left words to the left
#' @slot right words to the right
#' @slot corpus the CWB corpus
#' @slot cpos the corpus positions
#' @slot table Object of class \code{"data.frame"} a table with the relevant information for kwic output
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot labels Object of class \code{"character"}
#' @slot categories Object of class \code{"character"}
#' 
#' @param x a kwic-class object
#' @param object an object of class \code{kwic}
#' @param meta sAttributes (character vector) with metainformation
#' @param table logical, whether to turn cpos data.table into data.frame for output
#' @param size integer, the subset size for sampling
#' @section Methods:
#'   \describe{
#'    \item{[}{indexing for seeing only some concordances}
#'    \item{show}{get kwic output}
#'   }
#'   
#' @name kwic-class
#' @docType class
#' @aliases kwic-class [,kwic,ANY,ANY,ANY-method [,kwic-method
#' @exportClass kwic
#' @examples
#' use("polmineR")
#' K <- kwic("GERMAPARLMINI", "Integration")
#' length(K)
#' K[1]
#' K[1:5]
#' @rdname kwic-class
#' @include Labels.R
setClass(
  "kwic",
  slots = c(
    corpus = "character",
    cpos = "data.table",
    metadata = "character",
    left = "numeric",
    right = "numeric",
    table = "data.frame",
    encoding = "character",
    labels = "Labels",
    categories = "character"
  )
)


#' @include kwic_class.R kwic_method.R
NULL

#' @rdname kwic-class
#' @docType method
#' @importFrom DT datatable formatStyle
setMethod("show", "kwic", function(object){
  lineview <- getOption("polmineR.lineview")
  if (lineview == FALSE){
    df <- object@table
    df[["hit_no"]] <- NULL
    retvalRaw <- datatable(df, escape = FALSE)
    retvalRaw <- formatStyle(retvalRaw, "node", color="blue", textAlign="center")
    retval <- formatStyle(retvalRaw, "left", textAlign="right")
  } else {
    object@table[["node"]] <- paste('<span style="color:steelblue">', object@table[["node"]], '</span>', sep="")
    object@table[["text"]] <- apply(object@table, 1, function(x) paste(x[c("left", "node", "right")], collapse=" "))
    for (x in c("left", "node", "right", "hit_no")) object@table[[x]] <- NULL
    retval <- DT::datatable(object@table, escape = FALSE)
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
    meta = metadata,
    left = x@table$left,
    node = x@table$node,
    right = x@table$right
  )
})


#' @rdname kwic-class
setMethod("length", "kwic", function(x) nrow(x@table) )

#' @rdname kwic-class
setMethod("sample", "kwic", function(x, size){
  hits_unique <- unique(x@cpos[["hit_no"]])
  if (size > length(hits_unique)){
    warning("argument size exceeds number of hits, returning original object")
    return(x)
  }
  x@cpos <- x@cpos[which(x@cpos[["hit_no"]] %in% sample(hits_unique, size = size))]
  x <- enrich(x, table = TRUE)
  x <- enrich(x, meta = x@metadata)
  x
  
})
