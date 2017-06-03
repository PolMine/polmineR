#' @include textstat_class.R bundle_class.R
NULL


#' Feature selection by comparison (S4 class).
#' 
#' object resulting from features-method
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
#' 
#' @param .Object an object
#' @param object an object
#' @rdname features-class
#' @name features-class
#' @docType class
#' @exportClass features
#' @author Andreas Blaette
setClass("features",
         representation(
           corpus = "character",
           pAttribute = "character",
           encoding = "character",
           stat = "data.table",
           sizeCoi = "numeric",
           sizeRef = "numeric",
           method = "character",
           included = "logical",
           call = "character"
         ),
         contains = c("textstat")
)

#' @rdname features-class
#' @exportClass featuresCooccurrences
setClass("featuresCooccurrences", contains=c("features", "textstat"))

#' @rdname features-class
#' @exportClass featuresNgrams
setClass("featuresNgrams", representation(n="integer"), contains=c("features", "textstat"))

#' @slot objects an object of class \code{list}
#' @rdname features-class
setClass("featuresBundle", slots = c(objects = "list"), contains = "bundle")



#' @exportMethod summary
#' @docType methods
#' @rdname features-class
setMethod("summary", "features", function(object){.statisticalSummary(object)})


#' @exportMethod show
#' @docType methods
#' @rdname features-class
setMethod("show", "features", function(object){
  cat("the statistics table has", nrow(object@stat), "rows\n")
  cat("pos attributest have been added: ")
  if ("pos" %in% colnames(object@stat)){
    cat("YES\n")
  } else {
    cat("NO\n")
  }
})


#' @rdname features-class
setMethod("summary", "featuresBundle", function(object){
  tab <- do.call(rbind, lapply(object@objects, function(x) summary(x)$no))
  colnames(tab) <- c("0.001", "0.005", "0.010", "0.050")
  tab
})

