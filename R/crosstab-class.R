#' crosstab S4 class
#' 
#' Class for storing crosstabulations of frequencies of queries
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{partitions}:}{Object of class \code{"data.frame"} with sizes of the partition sizes for combinations of s-attributes analyzed }
#'     \item{\code{abs}:}{Object of class \code{"data.frame"} for each query: a data frame with absolute frequencies }
#'     \item{\code{rel}:}{Object of class \code{"data.frame"} for each query: a data frame with relative frequencies }
#'     \item{\code{rows}:}{Object of class \code{"character"} what you find in the rows }
#'     \item{\code{cols}:}{Object of class \code{"character"} what you find in the columns }
#'     \item{\code{query}:}{Object of class \code{"character"} the original queries }
#'   }
#'   
#' @section Methods:
#'    \describe{
#'     \item{show}{get summary of the object}
#'     \item{t}{change rows and columns}
#'    }
#'     
#' @name crosstab-class
#' @aliases show,crosstab-method t,crosstab-method
#' @seealso \code{dispersion}
#' @exportClass crosstab
#' @docType class
#' @rdname crosstab-class
setClass("crosstab",
         representation(partitions="data.frame", 
                        abs="data.frame",
                        rel="data.frame",
                        rows="character",
                        cols="character",
                        query="character"
         )
)
