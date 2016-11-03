#' Class for token stream operations.
#' 
#' @slot tokenStream a data.table that will include a column "token"
#' @slot corpus the CWB corpus (character vector length 1)
#' @slot encoding the encoding of the CWB corpus (character vector length 1)
#' @exportClass TokenStream
setClass(
  "TokenStream",
  representation = list(
    tokenStream = "data.table",
    corpus = "character",
    encoding = "character"
  )
)