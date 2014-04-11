#' CQP query object
#' 
#' Object to store and check a CQP query, and to supply a few methods.
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{query}:}{Object of class \code{"character"} the query }
#' }
#' 
#' @section Methods:
#'   \describe{
#'    \item{[}{get frequency of a query}
#'    }
#' 
#' @aliases cqpQuery-class [,cqpQuery-method [[,cqpQuery-method
#' @rdname cqpQuery-class
#' @name cqpQuery-class
#' @exportClass
#' @docType class
#' @author Andreas Blaette
setClass("cqpQuery",
         representation(query="character")
)


#' Convert a string to a CQP query
#' 
#' Takes a simple string as an imput and converts it to a valid CQP query
#' @param queries a character vector
#' @return a character vector
#' @export
#' @rdname as.cqpQuery
#' @name as.cqpQuery
as.cqpQuery <- function(queries){
  query <- new("cqpQuery")
  cqp <- vapply(
    queries,
    function(x) paste(
      vapply(
        unlist(strsplit(x, "\\s")),
        function(x) paste('"', x, '"', sep=''),
        USE.NAMES=FALSE,
        FUN.VALUE="character"
      ),
      collapse=" "
    ),
    FUN.VALUE="character",
    USE.NAMES=FALSE      
  )
  query@query <- cqp
  return(query)
}

setMethod('[', 'cqpQuery', function(x,i){
  hits <- nrow(.queryCpos(x, i))
  hits
})

setMethod('[[', 'cqpQuery', function(x,i){
  context <- context(
    x, i,
    get('drillingControls', '.GlobalEnv')[['pAttribute']],
    get('drillingControls', '.GlobalEnv')[['leftContext']],
    get('drillingControls', '.GlobalEnv')[['rightContext']],
    get('drillingControls', '.GlobalEnv')[['minSignificance']],
    get('drillingControls', '.GlobalEnv')[['posFilter']],
    verbose=FALSE
  )
  context
}
)

