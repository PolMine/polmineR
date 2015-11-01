#' kwic (S4 class)
#' 
#' S4 class for organizing information for concordance output
#' 
#' @slot metadata Object of class \code{"character"} keeping the sAttributes of the metadata that are to be displayed
#' @slot leftContext words to the left
#' @slot rightContext words to the right
#' @slot table Object of class \code{"data.frame"} a table with the relevant information for kwic output
#' @slot neighbor Object of class \code{"character"} neighbor, if applicable
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' 
#' @param x a kwic-class object
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
         slots=c(metadata="character",
                        leftContext="numeric",
                        rightContext="numeric",
                        neighbor="character",
                        table="data.frame",
                        encoding="character"
         )
)
