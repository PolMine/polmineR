#' Interfaces for accessing the CWB
#' 
#' The package offers three different interfaces to the Corpus
#' Workbench (CWB): The package 'rcqp', via cqpserver, and by
#' calling Perl scripts. An object called 'CQI' will be instantiated
#' in the environment of the polmineR package; the class will 
#' provide the functionality to access CWB corpora.
#' @rdname CQI
#' @export CQI
#' @importFrom R6 R6Class
#' @aliases CQI
CQI.super <- R6Class(
  "CQI.super",
  public = list(
    test = function()message("hi there")
  )
)
