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
#' @aliases cqpQuery-class [,cqpQuery-method [[,cqpQuery-method show,cqpQuery-method
#' @rdname cqpQuery-class
#' @name cqpQuery-class
#' @exportClass cqpQuery
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
#' @export as.cqpQuery
#' @rdname as.cqpQuery
#' @name as.cqpQuery
as.cqpQuery <- function(queries){
  cqp <- c()
  for (query in queries){
    bag <- c()
    for (q in unlist(strsplit(query, "\\s"))){
        if (substr(q, 1, 1) !='['){
          q <- paste('"', q, '"', sep='') 
        }
        bag <- append(bag, q)      
    }
    cqp <- append(cqp, paste('(', paste(bag, sep='', collapse=' '), ')', sep=""))
  }
  if (length(cqp)>1){
    cqp <- paste('(', paste(cqp, sep='', collapse='|'), ')', sep="")
  }    
  cqpQuery <- new("cqpQuery")
  cqpQuery@query <- cqp
  return(cqpQuery)
}

setMethod('[', 'cqpQuery', function(x,i){
  hits <- nrow(.queryCpos(x@query, i))
  hits
})

setMethod('[[', 'cqpQuery', function(x,i){
  context <- context(
    x@query, i,
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

#' @exportMethod show
#' @noRd
setMethod('show', 'cqpQuery', function(object){
  cat("A 'cqpQuery' object. The query is:\n")
  cat(object@query, '\n')
})