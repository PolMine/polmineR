#' #' @rdname coerce-methods
#' setGeneric("as.dfm", function(.Object) standardGeneric("as.dfm"))
#' 
#' #' @rdname coerce-methods
#' #' @importClassesFrom quanteda dfmSparse
#' setMethod("as.dfm", "TermDocumentMatrix", function(.Object){
#'   if (requireNamespace("quanteda", quietly=TRUE)){
#'     new("dfmSparse", as.sparseMatrix(.Object))  
#'   } else {
#'     stop("package 'quanteda required but not available")
#'   }
#' })
#' 
#' #' @importClassesFrom quanteda dfmSparse
#' #' @rdname coerce-methods
#' setMethod("as.dfm", "DocumentTermMatrix", function(.Object){
#'   if (requireNamespace("quanteda", quietly=TRUE)){
#'     new("dfmSparse", as.sparseMatrix(t(.Object)))
#'   } else {
#'     stop("package 'quanteda required but not available")
#'   }
#' })
