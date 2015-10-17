#' @include keyness_class.R

#' Summary of a keyness object
#' 
#' @exportMethod summary
#' @docType methods
#' @noRd
setMethod("summary", "keyness", function(object){.statisticalSummary(object)})


#' Information on a keyness object
#' 
#' @exportMethod show
#' @docType methods
#' @noRd
setMethod("show", "keyness", function(object){
    cat("the statistics table has", nrow(object@stat), "rows\n")
    cat("pos attributest have been added: ")
    if ("pos" %in% colnames(object@stat)){
      cat("YES\n")
    } else {
      cat("NO\n")
    }
  })
