#' @include bundle-class.R
NULL

setClass("keynessCluster",
         slots=c(objects="list"),
         contains=c("bundle")
)

