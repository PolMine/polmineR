#' Labels class.
#' 
#' @field labels character vector with labels; if logical or numeric labels
#' are intended, assign them as character vector anyway
#' @field choices character vector, a list of choices for labels 
#' @field expandable whether choices may be expanded (logical)
#' @param n length of character vector in field labels
#' @param choices choices to be assigned to field choices
#' @param expandable whether choices are expandable
Labels <- setRefClass(
  
  Class = "Labels",
  
  fields = list(
    
    labels = "character",
    choices = "character",
    expandable = "logical"
    
  ),
  
  methods = list(
    
    initialize = function(n, choices = c("TRUE", "FALSE", NA), expandable = FALSE){
      .self$labels <- rep(as.character(NA), times = n)
      .self$choices <- choices
      .self$expandable <- expandable
    }
    
  )
)