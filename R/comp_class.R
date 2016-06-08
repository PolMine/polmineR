#' @include textstat_class.R
NULL


#' S4 class for comparing corpora
#' 
#' object resulting from compare-method
#' 
#' @slot corpus Object of class \code{"character"} 
#' @slot pAttribute Object of class \code{"character"} 
#' @slot encoding Object of class \code{"character"}  
#' @slot corpus Object of class \code{"character"}  
#' @slot stat Object of class \code{"data.frame"} 
#' @slot sizeCoi Object of class \code{"numeric"} 
#' @slot sizeRef Object of class \code{"numeric"} 
#' @slot included Object of class \code{"logical"} whether corpus of interest is included in reference corpus
#' @slot method Object of class \code{"character"} statisticalTest used
#' @slot call Object of class \code{"character"} the call that generated the object
#'  @section Methods:
#'   \describe{
#'    \item{summary}{\code{signature(object = "comp")}: Display essential information }
#'    \item{addPos}{\code{signature(object = "comp")}: add POS attribute to statistics table }
#'    \item{as.data.frame}{\code{signature(object = "comp")}: get the statistics table}
#'    }
#' @param .Object an object
#' @param object an object
#' @rdname comp-class
#' @name comp-class
#' @docType class
#' @exportClass comp
#' @author Andreas Blaette
setClass("comp",
         representation(
           corpus="character",
           pAttribute="character",
           encoding="character",
           stat="data.table",
           sizeCoi="numeric",
           sizeRef="numeric",
           method="character",
           included="logical",
           call="character"
         ),
         contains=c("textstat")
)

#' @rdname comp-class
#' @exportClass compCooccurrences
setClass("compCooccurrences", contains=c("comp", "textstat"))

#' @rdname comp-class
#' @exportClass compNgrams
setClass("compNgrams", representation(n="integer"), contains=c("comp", "textstat"))

#' @slot objects an object of class \code{list}
#' @rdname comp-class
setClass("compBundle", slots=c(objects="list"), contains=c("bundle"))



#' @exportMethod summary
#' @docType methods
#' @rdname comp-class
setMethod("summary", "comp", function(object){.statisticalSummary(object)})


#' @exportMethod show
#' @docType methods
#' @rdname comp-class
setMethod("show", "comp", function(object){
  cat("the statistics table has", nrow(object@stat), "rows\n")
  cat("pos attributest have been added: ")
  if ("pos" %in% colnames(object@stat)){
    cat("YES\n")
  } else {
    cat("NO\n")
  }
})


#' @rdname comp-class
setMethod("summary", "compBundle", function(object){
  tab <- do.call(rbind, lapply(object@objects, function(x) summary(x)$no))
  colnames(tab) <- c("0.001", "0.005", "0.010", "0.050")
  tab
})

