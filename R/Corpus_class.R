setOldClass("Corpus")


#' Corpus class.
#' 
#' @field corpus character vector, CWB corpus
#' @field encoding encoding of the corpus
#' @field count data.table with counts
#' @section Methods:
#' \describe{
#'   \item{one}{bla}
#' }
#' 
#' @rdname Corpus-class
#' @export Corpus
Corpus <- R6Class(
  "Corpus",
  public = list(
    
    corpus = NULL,
    encoding = NULL,
    pAttribute = NULL,
    size = NULL,
    stat = data.table(),
    
    initialize = function(corpus, pAttribute = NULL){
      if (!corpus %in% polmineR::corpus()[["corpus"]]) warning("corpus may not be available")
      self$corpus <- corpus
      self$encoding <- getEncoding(corpus)
      self$size <- size(corpus)
      if (!is.null(pAttribute)) self$count(pAttribute)
    }, 
    
    count = function(pAttribute = getOption("polmineR.pAttribute")){
      self$pAttribute <- pAttribute
      self$stat <- count(self$corpus, pAttribute = pAttribute)
    },
    
    as.partition = function(){
      new(
        "partition",
        corpus = self$corpus,
        encoding = self$encoding,
        cpos = matrix(c(0, (size(self$corpus) - 1)), nrow = 1),
        stat = self$stat,
        size = self$size,
        pAttribute = self$pAttribute
      )
    }
  )
)

