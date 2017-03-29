#' Conversion between corpus and native encoding.
#' 
#' Utility functions to convert encoding between the native encoding and the 
#' encoding of the corpus.
#' 
#' @param x the object (a character vector)
#' @param corpusEnc the encoding of the corpus (e.g. "latin1", "UTF-8")
#' @rdname encodings
#' @export as.utf8
#' @rdname encodings
as.utf8 <- function(x, from = "latin1"){
  Encoding(x) <- from
  y <- enc2utf8(x)
  Encoding(y) <- "unknown"
  y
}

#' @export as.nativeEnc
#' @rdname encodings
as.nativeEnc <- function(x, corpusEnc){
  Encoding(x) <- corpusEnc
  y <- enc2native(x)
  Encoding(y) <- "unknown"
  y
}

#' @export as.corpusEnc
#' @rdname encodings
as.corpusEnc <- function(x, corpusEnc){
  y <- iconv(x, from = localeToCharset()[1], to = corpusEnc)
  Encoding(y) <- corpusEnc
  y
}
