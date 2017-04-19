#' Conversion between corpus and native encoding.
#' 
#' Utility functions to convert encoding between the native encoding and the 
#' encoding of the corpus.
#' 
#' The encoding of a corpus and the encoding of the terminal (the native 
#' encoding) may differ and evoke strange output, or wrong results if no
#' conversion is carried out between the potentially differing encodings. The
#' functions \code{as.nativeEnc} and \code{as.corpusEnc} are auxiliary functions
#' to assist this. The functions \code{as.nativeEnc} and \code{as.utf8}
#' deliberately remove the explicit statement of the encoding, to avoid warnings
#' that may occur with character vector columns in a \code{data.table} object.
#' 
#' @param x the object (a character vector)
#' @param from encoding of the input character vector
#' @param corpusEnc encoding of the corpus (e.g. "latin1", "UTF-8")
#' @rdname encodings
#' @export as.utf8
#' @rdname encodings
#' @name encodings
as.utf8 <- function(x, from){
  Encoding(x) <- from
  y <- enc2utf8(x)
  Encoding(y) <- "unknown"
  y
}

#' @export as.nativeEnc
#' @rdname encodings
as.nativeEnc <- function(x, from){
  Encoding(x) <- from
  y <- enc2native(x)
  Encoding(y) <- "unknown"
  y
}

#' @export as.corpusEnc
#' @rdname encodings
#' @importFrom utils localeToCharset
as.corpusEnc <- function(x, from = localeToCharset()[1], corpusEnc){
  y <- iconv(x, from = from, to = corpusEnc)
  Encoding(y) <- corpusEnc
  y
}
