#' @include S4classes.R
NULL

#' Get and set encoding.
#' 
#' Method for \code{textstat} objects and classes inheriting from
#' \code{textstat}; if \code{object} is a character vector, the encoding of the
#' corpus is returned..
#' 
#' @param object A \code{textstat} or \code{bundle} object, or a length-one
#'   character vector specifying a corpus.
#' @param value Value to be assigned.
#' @rdname encoding
#' @exportMethod encoding
#' @examples
#' # Get encoding of a corpus.
#' encoding("REUTERS")
#' 
#' # Get encoding of a partition.
#' r <- partition("REUTERS", places = "kuwait", regex = TRUE)
#' encoding(r)
#' 
#' # Get encoding of another class inheriting from textstat (count).
#' cnt <- count("REUTERS", p_attribute = "word")
#' encoding(cnt)
#' 
#' # Get encoding of objects in a bundle.
#' pb <- partition_bundle("REUTERS", s_attribute = "id")
#' encoding(pb)
setGeneric("encoding", function(object) standardGeneric("encoding"))

#' @rdname encoding
setGeneric("encoding<-", function(object, value) standardGeneric("encoding<-"))

#' @rdname encoding
setMethod("encoding", "textstat", function(object) object@encoding)

#' @rdname encoding
setMethod("encoding", "bundle", function(object) object@encoding)

#' @rdname encoding
setMethod("encoding", "character", function(object) registry_get_encoding(object))

#' @rdname encoding
setMethod("encoding", "corpus", function(object) object@encoding)

#' @rdname encoding
setMethod("encoding", "subcorpus", function(object) callNextMethod())

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
  y <- enc2native(x) # has been benchmarked, is faster than iconv and stringi::stri_encode
  Encoding(y) <- "unknown"
  y
}

#' @export as.corpusEnc
#' @rdname encodings
#' @importFrom utils localeToCharset
as.corpusEnc <- function(x, from = localeToCharset()[1], corpusEnc){
  if (is.na(from)) from <- "UTF-8"
  y <- iconv(x, from = from, to = corpusEnc)
  Encoding(y) <- corpusEnc
  y
}

#' @examples
#' y <- .recode_call(substitute(name == "Müller"), to = "latin1")
#' dt <- data.table(
#'   id = 1L:3L,
#'   name = iconv(x = c("Müller", "Höhn", "Delingöz"), from = "UTF-8", to = "latin1")
#' )
#' dt[eval(y, envir = df), on = "name"]
#' @noRd
.recode_call <- function(x, from = localeToCharset()[1], to){
  .fn <- function(x){
    if (is.call(x)){
      return( as.call(lapply(x, .fn)) )
    } else if (is.character(x)){
      return( iconv(x = x, from = from, to = to) )
    } else {
      return(x)
    }
  }
  .fn(x)
}