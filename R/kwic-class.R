#' kwic (S4 class)
#' 
#' S4 class for organizing information for concordance output
#' 
#' @section Slots:
#'   \describe{
#'    \item{\code{metadata}:}{Object of class \code{"character"} keeping the sAttributes of the metadata that are to be displayed }
#'    \item{\code{table}:}{Object of class \code{"data.frame"} a table with the relevant information for kwic output }
#'    \item{\code{collocate}:}{Object of class \code{"character"} collocate, if applicable }
#'    \item{\code{encoding}:}{Object of class \code{"character"} encoding of the corpus }
#'   }
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
#' @rdname kwic-class
setClass("kwic",
         representation(metadata="character",
                        collocate="character",
                        table="data.frame",
                        encoding="character"
         )
)
