shinyPartition <- "UNSET"
shinyReferencePartition <- "UNSET"

#' Shiny app for contextual analysis
#' 
#' Fires up a shiny app for analysing the context of a node.
#' The default values of \code{drillingControls} are used.
#'  
#' Shiny will open a window in your browser, offering to perform a context analysis
#' based on the partition objects found in the global environment.
#' 
#' @return not applicable
#' @importFrom shiny runApp
#' @export shinyContext
#' @author Andreas Blaette
shinyContext <- function() {
  assignInNamespace('drillingControls', get('drillingControls', '.GlobalEnv'), 'polmineR')
#  shiny::runApp(system.file("shinyAppContext", package="driller"))
  shiny::runApp('/media/Daten/lab/repos/driller/inst/shinyAppContext')
}

#' Shiny app for keyness statistics
#' 
#' Fires up a shiny app. The default values of \code{drillingControls} are used.
#'  
#' Shiny will open a window in your browser. To other partitions, the function
#' has to be called again.
#' 
#' @param partition1 a partition object (corpus of interest)
#' @param partition2 a partition object (reference corpus)
#' @return not applicable
#' @export shinyKeyness
#' @author Andreas Blaette
shinyKeyness <- function(partition1, partition2) {
  assignInNamespace('drillingControls', get('drillingControls', '.GlobalEnv'), 'polmineR')
  #  shiny::runApp(system.file("shinyAppKeyness", package="driller"))
  shiny::runApp('/media/Daten/lab/repos/driller/inst/shinyAppKeyness')
}

#' Shiny app for exploring the distribution
#' 
#' Fires shiny apps for analysing the distribution of a query 
#' across subcorpora. This is a wrapper!
#'  
#' Shiny will open a window in your browser.
#' 
#' @param type either "simple", "multi" or "crosstab"
#' @return not applicable
#' @export shinyDispersion
#' @author Andreas Blaette
shinyDispersion <- function(type) {
  assignInNamespace('drillingControls', get('drillingControls', '.GlobalEnv'), 'polmineR')
  if (type=="simple") {
    shiny::runApp('/media/Daten/lab/repos/driller/inst/shinyAppDistribution')
  } else if (type == "crosstab") {
    shiny::runApp('/media/Daten/lab/repos/driller/inst/shinyAppCrosstab')
  } else if (type == "multi"){
    shiny::runApp('/media/Daten/lab/repos/driller/inst/shinyAppMulti')    
  }
}
#' Shiny app for contextual analysis
#' 
#' Fires up a shiny app for analysing the context of a node.
#' The default values of \code{drillingControls} are used.
#'  
#' Shiny will open a window in your browser, offering to perform a context analysis
#' based on the partition objects found in the global environment.
#' 
#' @return not applicable
#' @author Andreas Blaette
#' @export shinyContext
shinyContext <- function() {
  assignInNamespace('drillingControls', get('drillingControls', '.GlobalEnv'), 'polmineR')
  #  shiny::runApp(system.file("shinyAppContext", package="driller"))
  shiny::runApp('/media/Daten/lab/repos/driller/inst/shinyAppContext')
}

#' Shiny app for volatility analysis
#' 
#' Fires up a shiny app. The default values of \code{drillingControls} are used.
#'  
#' Shiny will open a window in your browser. To other partitions, the function
#' has to be called again.
#' 
#' @author Andreas Blaette
#' @export shinyVolatility
shinyVolatility <- function() {
  assignInNamespace('drillingControls', get('drillingControls', '.GlobalEnv'), 'polmineR')
  #  shiny::runApp(system.file("shinyAppKeyness", package="driller"))
  shiny::runApp('/media/Daten/lab/repos/driller/inst/shinyAppVolatility')
}
