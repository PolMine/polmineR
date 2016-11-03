#' @include partition_class.R
NULL


#' Regions of a CWB corpus.
#' 
#' @slot cpos a data.table that will include a "cpos_left" and "cpos_right" column
#' @slot corpus the CWB corpus (character vector length 1)
#' @slot encoding the encoding of the CWB corpus (character vector length 1)
#' @exportClass Regions
setClass(
  "Regions",
  representation = list(
    cpos = "data.table",
    corpus = "character",
    encoding = "character"
  )
)

setAs(from = "partition", to = "Regions", function(from, to){
  new(
    "Regions",
    cpos = data.table(
      cpos_left = from@cpos[,1],
      cpos_right = from@cpos[,2]
    ),
    encoding = from@encoding,
    corpus = from@corpus
    )
})

