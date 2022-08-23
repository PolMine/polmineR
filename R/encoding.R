#' @include S4classes.R
NULL

#' Get and set encoding.
#'
#' Method for \code{textstat} objects and classes inheriting from
#' \code{textstat}; if \code{object} is a character vector, the encoding of the
#' corpus is returned. If called without arguments, the session character set is
#' returned.
#' 
#' @param object A \code{textstat} or \code{bundle} object (or an object
#'   inheriting from these classes), or a length-one \code{character} vector
#'   specifying a corpus. If missing, the method will return the session
#'   character set.
#' @param value Value to be assigned.
#' @rdname encoding
#' @return A length-one \code{character} vector with an encoding.
#' @exportMethod encoding
#' @examples
#' # Get session charset.
#' encoding()
#' 
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
#' @exportMethod encoding<-
setGeneric("encoding<-", function(object, value) standardGeneric("encoding<-"))

#' @details `encoding()` uses `l10n_info()` and `localeToCharset()` (in this
#'   order) to determine the session encoding. If `localeToCharset()` returns
#'   `NA`, "UTF-8" is assumed to be the session encoding.
#' @rdname encoding
setMethod("encoding", "missing", function(object){
  y <- if (l10n_info()[["UTF-8"]]) "UTF-8" else localeToCharset()[1]
  if (is.na(y)) "UTF-8" else y
})


#' @rdname encoding
setMethod("encoding", "textstat", function(object) object@encoding)

#' @rdname encoding
setMethod("encoding", "bundle", function(object) object@encoding)

#' @rdname encoding
setMethod("encoding", "character", function(object) corpus(object)@encoding)

#' @rdname encoding
setMethod("encoding", "corpus", function(object) object@encoding)

#' @rdname encoding
setMethod("encoding", "subcorpus", function(object) callNextMethod())


#' Conversion between corpus and native encoding.
#'
#' Utility functions to convert the encoding of a \code{character} vector
#' between the native encoding and the encoding of the corpus.
#'
#' The encoding of a corpus and the encoding of the terminal (the native
#' encoding) may differ, provoking strange or wrong results if no conversion is
#' carried out between the potentially differing encodings. The functions
#' \code{as.nativeEnc()} and \code{as.corpusEnc} are auxiliary functions to
#' assist the conversion. The functions \code{as.nativeEnc} and \code{as.utf8}
#' deliberately remove the explicit statement of the encoding, to avoid warnings
#' that may occur with character vector columns in a \code{data.table} object.
#'
#' @param x A \code{character} to be converted.
#' @param from A \code{character} vector describing the encoding of the input
#'   character vector.
#' @param corpusEnc A \code{character} vector describing the target encoding,
#'   i.e. the encoding of the corpus (usually "latin1", "UTF-8")
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
as.corpusEnc <- function(x, from = encoding(), corpusEnc){
  x <- as.character(x)
  if (is.na(from)) from <- "UTF-8"
  
  inputenc <- unique(Encoding(x))
  if (any(!inputenc %in% c("unknown", from))){
    warning(
      sprintf(
        "Found '%s' encoded strings, an unexpected encoding that may result in warnings and unpredictable behavior.  Please check input!",
        inputenc[!inputenc %in% c("unknown", from)])
      )
  }
  
  y <- iconv(x, from = from, to = corpusEnc)
  if (anyNA(y)){
    for (string in x){
      if (is.na(iconv(string, from = from, to = corpusEnc))){
        chars <- strsplit(string, split = "")[[1]]
        inconvertible <- chars[is.na(iconv(chars, from = from, to = corpusEnc))]
        warning(
          sprintf(
            paste(
              c(
              "Character string '%s' to be converted from encoding '%s' to targert encoding '%s'",
              "contains single characters (%s) that are non-convertible.",
              "The result is an NA character vector which may provoke errors.",
              "Recommended solution:",
              "Substitute non-convertible single characters with an appropriate equivalent",
              "or a regular expression metacharacter such as '.'."
              ),
              collapse = " "
            ),
            string, from, corpusEnc, paste(sprintf("'%s'", inconvertible), collapse = "/")
          )
        )
      }
    }
  }
  Encoding(y) <- corpusEnc
  y
}


#' @rdname encoding
setMethod("encoding", "call", function(object){
  .fn <- function(x){
    if (is.call(x)){
      return( lapply(x, .fn) )
    } else if (is.character(x)){
      return( Encoding(x) )
    } else {
      return(NULL)
    }
  }
  y <- unique(unlist(.fn(object)))
  if ("unknown" %in% y) y <- y[-which(y == "unknown")]
  if (length(y) == 0L) y <- "unknown"
  y
})

#' @rdname encoding
setMethod("encoding", "quosure", function(object){
  encoding(quo_get_expr(object))
})


.recode <- function(x, from = encoding(), to){
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


#' @rdname encoding
setReplaceMethod("encoding", signature = "call", function(object, value){
  enc <- encoding(object)
  if (enc != "unknown") object <- .recode(x = object, from = enc, to = value)
  object
})

#' @importFrom rlang quo_set_expr
#' @rdname encoding
setReplaceMethod("encoding", signature = "quosure", function(object, value){
  enc <- encoding(object)
  if (enc != "unknown"){
    expr <- quo_get_expr(object)
    encoding(expr) <- value
    object <- quo_set_expr(object, expr = expr)
  }
  object
})

