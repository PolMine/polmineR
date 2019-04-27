#' Labels class.
#' 
#' @field labels character vector with labels; if logical or numeric labels
#' are intended, assign them as character vector anyway
#' @field choices A \code{character} vector, a list of choices for labels.
#' @field expandable Whether choices may be expanded (\code{logical}).
#' @param n length of character vector in field labels
#' @param choices choices to be assigned to field choices
#' @param default The default value for the labels.
#' @param expandable whether choices are expandable
Labels <- setRefClass(
  
  Class = "Labels",
  
  fields = list(
    
    labels = "character",
    choices = "character",
    expandable = "logical"
    
  ),
  
  methods = list(
    
    initialize = function(n, choices = c("TRUE", "FALSE", NA), default = choices[1], expandable = FALSE){
      .self$labels <- rep(default, times = n)
      .self$choices <- choices
      .self$expandable <- expandable
    }
    
  )
)