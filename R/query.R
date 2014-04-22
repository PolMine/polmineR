#' Convert a string to a CQP query
#' 
#' Takes a simple string as an imput and converts it to a valid CQP query
#' @param queries a character vector
#' @param collapse whether collapse the queries into one
#' @return a character vector
#' @export as.cqpQuery
#' @rdname as.cqpQuery
#' @name as.cqpQuery
as.cqpQuery <- function(queries, collapse=FALSE){
  cqp <- c()
  for (query in queries){
    bag <- c()
    for (q in unlist(strsplit(query, "\\s"))){
        if ((substr(q, 1, 1) !='[') && (substr(q, nchar(q), nchar(q)) != ']')){
          q <- paste('"', q, '"', sep='') 
        }
        bag <- append(bag, q)      
    }
    cqp <- append(cqp, paste('(', paste(bag, sep='', collapse=' '), ')', sep=""))
  }
  if (length(cqp)>1 && collapse==TRUE){
    cqp <- paste('(', paste(cqp, sep='', collapse='|'), ')', sep="")
  }    
  return(cqp)
}
