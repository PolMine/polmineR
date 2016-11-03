#' kwic (S4 class)
#' 
#' S4 class for organizing information for concordance output
#' 
#' @slot metadata Object of class \code{"character"} keeping the sAttributes of the metadata that are to be displayed
#' @slot left words to the left
#' @slot right words to the right
#' @slot corpus the CWB corpus
#' @slot cpos the corpus positions
#' @slot table Object of class \code{"data.frame"} a table with the relevant information for kwic output
#' @slot neighbor Object of class \code{"character"} neighbor, if applicable
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot labels Object of class \code{"character"}
#' @slot categories Object of class \code{"character"}
#' 
#' @param x a kwic-class object
#' @param object an object of class \code{kwic}
#' @param meta sAttributes (character vector) with metainformation
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
setClass(
  "kwic",
  slots = c(
    corpus = "character",
    cpos = "list",
    metadata = "character",
    left = "numeric",
    right = "numeric",
    neighbor = "character",
    table = "data.frame",
    encoding = "character",
    labels = "character",
    categories = "character"
  )
)
