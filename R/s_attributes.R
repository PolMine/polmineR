#' @include partition.R S4classes.R
NULL

#' Get s-attributes.
#' 
#' Structural annotations (s-attributes) of a corpus capture metainformation for
#' regions of tokens. The \code{s_attributes}-method offers high-level access to
#' the s-attributes present in a \code{corpus} or \code{subcorpus}, or the values of
#' s-attributes in a \code{corpus}/\code{partition}.
#' 
#' Importing XML into the Corpus Workbench (CWB) turns elements and element
#' attributes into so-called "s-attributes". There are two basic uses of the
#' \code{s_attributes}-method: If the argument \code{s_attribute} is \code{NULL}
#' (default), the return value is a \code{character} vector with all
#' s-attributes present in a corpus.
#' 
#' If \code{s_attribute} is the name of a specific s-attribute (a length one
#' character vector), the values of the s-attributes available in the
#' \code{corpus}/\code{partition} are returned.
#' 
#' If argument \code{unique} is \code{FALSE}, the full sequence of the
#' s_attributes is returned, which is a useful building block for decoding a
#' corpus.
#' 
#' If a character vector including several s-attributes is provided, the method
#' will return a \code{data.table}.
#'
#' @param .Object A \code{corpus}, \code{subcorpus}, \code{partition} object, or
#'   a \code{call}. A corpus can also be specified by a length-one character
#'   vector.
#' @param s_attribute The name of a specific s-attribute.
#' @param unique Logical, whether to return unique values.
#' @param regex A regular expression passed into \code{grep} to filter return
#'   value by applying a regex.
#' @param ... To maintain backward compatibility, if argument \code{sAttribute}
#'   (deprecated) is used. If \code{.Object} is a \code{remote_corpus} or
#'   \code{remote_subcorpus} object, the three dots (\code{...}) are used to
#'   pass arguments. Hence, it is necessary to state the names of all arguments
#'   to be passed explicity.
#' @return A character vector (s-attributes, or values of s-attributes).
#' @exportMethod s_attributes
#' @docType methods
#' @rdname s_attributes-method
#' @examples 
#' use("polmineR")
#' @rdname s_attributes-method
#' @name s_attributes
setGeneric("s_attributes", function(.Object, ...) standardGeneric("s_attributes"))


#' @rdname s_attributes-method
#' @aliases s_attributes,character-method
#' @examples 
#' 
#' s_attributes("GERMAPARLMINI")
#' s_attributes("GERMAPARLMINI", "date") # dates of plenary meetings
#' s_attributes("GERMAPARLMINI", s_attribute = c("date", "party"))  
setMethod("s_attributes", "character", function(.Object, s_attribute = NULL, unique = TRUE, regex = NULL, ...){
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  s_attributes(.Object = corpus(.Object), s_attribute = s_attribute, unique = unique, regex = regex, ...)
})


#' @rdname s_attributes-method
#' @examples
#' s_attributes(corpus("GERMAPARLMINI"))
setMethod("s_attributes", "corpus", function(.Object, s_attribute = NULL, unique = TRUE, regex = NULL, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (is.null(s_attribute)){
    return( registry_get_s_attributes(corpus = .Object@corpus, registry = registry()) )
  } else {
    if (length(s_attribute) == 1L){
      avs_file <- file.path(.Object@data_dir, paste(s_attribute, "avs", sep = "."))
      avs_file_size <- file.info(avs_file)[["size"]]
      avs <- readBin(con = avs_file, what = character(), n = avs_file_size)
      Encoding(avs) <- .Object@encoding
      if (.Object@encoding != localeToCharset()[1]) avs <- as.nativeEnc(avs, from = .Object@encoding)
      
      if (unique){
        return(avs)
      } else {
        avx_file <- file.path(.Object@data_dir, paste(s_attribute, "avx", sep = "."))
        avx_file_size <- file.info(avx_file)[["size"]]

        avx <- readBin(avx_file, what = integer(), size = 4L, n = avx_file_size / 4L, endian = "big")
        avx_matrix <- matrix(avx, ncol = 2, byrow = TRUE)

        y <- avs[match(avx_matrix[, 2], unique(avx_matrix[, 2]))]
        
        if (!is.null(regex)) y <- grep(regex, y, value = TRUE)
        return(y)
      }
    } else if (length(s_attribute) > 1L){
      y <- lapply(
        s_attribute,
        function(x) {
          retval <- cl_struc2str(corpus = .Object@corpus, s_attribute = x, struc = 0L:(cl_attribute_size(corpus = .Object@corpus, attribute = x, attribute_type = "s", registry = registry()) - 1L), registry = registry())
          Encoding(retval) <- .Object@encoding
          as.nativeEnc(retval, from = .Object@encoding)
        })
      names(y) <- s_attribute
      return( data.table::as.data.table(y) )
    }
  }
})


#' @rdname s_attributes-method
#' @examples
#' p <- partition("GERMAPARLMINI", date = "2009-11-10")
#' s_attributes(p)
#' s_attributes(p, "speaker") # get names of speakers
setMethod(
  "s_attributes", "slice",
  function (.Object, s_attribute = NULL, unique = TRUE, ...) {
    if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
    if (is.null(s_attribute)){
      return( registry_get_s_attributes(.Object@corpus) )
    } else {
      if (length(s_attribute) == 1L){
        # Checking whether the xml is flat / whether s_attribute is in .Object@s_attribute_strucs 
        # is necessary because there are scenarios when these slots are not defined.
        xml_is_flat <- if (length(.Object@xml) > 0L) if (.Object@xml == "flat") TRUE else FALSE else FALSE
        s_attr_strucs <- if (length(.Object@s_attribute_strucs) > 0L) if (.Object@s_attribute_strucs == s_attribute) TRUE else FALSE else FALSE
        if (xml_is_flat && s_attr_strucs){
          len1 <- cl_attribute_size(corpus = .Object@corpus, attribute = .Object@s_attribute_strucs, attribute_type = "s", registry = registry())
          len2 <- cl_attribute_size(corpus = .Object@corpus, attribute = s_attribute, attribute_type = "s", registry = registry())
          if (len1 != len2){
            stop("XML is stated to be flat, but s_attribute_strucs hat length ", len1, " and s_attribute length ", len2)
          }
          retval <- cl_struc2str(corpus = .Object@corpus, s_attribute = s_attribute, struc = .Object@strucs, registry = registry())
          if (unique) retval <- unique(retval)
        } else {
          cpos_vector <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
          strucs <- cl_cpos2struc(
            corpus = .Object@corpus,
            s_attribute = s_attribute,
            cpos = cpos_vector,
            registry = registry()
          )
          strucs <- unique(strucs)
          # filtering out negative struc values is necessary, because RcppCWB
          # will complain about negative values
          strucs <- strucs[which(strucs >= 0L)]
          retval <- cl_struc2str(corpus = .Object@corpus, s_attribute = s_attribute, struc = strucs, registry = registry())
          if (unique) retval <- unique(retval)
        }
        Encoding(retval) <- .Object@encoding
        retval <- as.nativeEnc(retval, from = .Object@encoding)
        Encoding(retval) <- localeToCharset()[1]
        return(retval)
      } else if (length(s_attribute) > 1L){
        if (.Object@xml == "flat") {
          tab <- data.frame(
            lapply(
              s_attribute,
              # USE.NAMES = TRUE,
              function(x) { 
                tmp <- cl_struc2str(corpus = .Object@corpus, s_attribute = x, struc = .Object@strucs, registry = registry())
                Encoding(tmp) <- .Object@encoding
                tmp <- as.nativeEnc(tmp, from = .Object@encoding)
                Encoding(tmp) <- localeToCharset()[1]
                tmp
              }
            ),
            stringsAsFactors = FALSE
          )
          colnames(tab) <- s_attribute
        } else if (.Object@xml == "nested") {
          tab <- data.frame(
            sapply(
              s_attribute,
              USE.NAMES = TRUE,
              function(x) {
                tmp <- cl_struc2str(corpus = .Object@corpus, s_attribute = x, struc = cl_cpos2struc(corpus = .Object@corpus, s_attribute = x, cpos = .Object@cpos[,1], registry = registry()), registry = registry())
                Encoding(tmp) <- .Object@encoding
                as.nativeEnc(tmp, from = .Object@encoding)
                Encoding(tmp) <- localeToCharset()[1]
                tmp
              }
            )
          )
          colnames(tab) <- s_attribute
        }
        return( data.table::as.data.table(tab) )
        
      }
    }
  }
)

#' @rdname s_attributes-method
setMethod("s_attributes", "partition", function (.Object, s_attribute = NULL, unique = TRUE, ...)
  callNextMethod()
)


#' @rdname s_attributes-method
setMethod("s_attributes", "subcorpus", function (.Object, s_attribute = NULL, unique = TRUE, ...)
  callNextMethod()
)



#' @docType methods
#' @rdname partition_bundle-class
setMethod("s_attributes", "partition_bundle", function(.Object, s_attribute, ...){
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  lapply(.Object@objects, function(x) s_attributes(x, s_attribute))
})


#' @details If \code{.Object} is a call, the \code{s_attributes}-method will
#'   return a character vector with the s-attributes occurring in the call. This
#'   usage is relevant internally to implement the \code{subset} method to
#'   generate a \code{subcorpus} using non-standard evaluation. Usually it will
#'   not be relevant in an interactive session.
#' @rdname s_attributes-method
#' @param corpus A \code{corpus}-object or a length one character vector
#'   denoting a corpus.
#' @examples
#' 
#' # Get s-attributes occurring in a call
#' s_attributes(quote(grep("Merkel", speaker)), corpus = "GERMAPARLMINI")
#' s_attributes(quote(speaker == "Angela Merkel"), corpus = "GERMAPARLMINI")
#' s_attributes(quote(speaker != "Angela Merkel"), corpus = "GERMAPARLMINI")
#' s_attributes(
#'   quote(speaker == "Angela Merkel" & date == "2009-10-28"),
#'   corpus = "GERMAPARLMINI"
#' )
setMethod("s_attributes", "call", function(.Object, corpus){
  s_attrs <- s_attributes(corpus)
  s_attrs <- if (is.character(corpus)){
    registry_get_s_attributes(corpus = corpus, registry = registry())
  } else {
    registry_get_s_attributes(corpus = corpus@corpus, registry = registry())
  }
  # for the following recursive function, see http://adv-r.had.co.nz/Expressions.html
  .fn <- function(x){
    if (is.call(x)){
      y <- lapply(x, .fn)
    } else if (is.symbol(x)){
      char <- deparse(x)
      y <- if (char %in% s_attrs) char else NULL
    } else {
      y <- NULL
    }
    unique(unlist(y))
  }
  .fn(.Object)
})


#' @rdname s_attributes-method
setMethod("s_attributes", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "s_attributes", server = .Object@server, do.call = FALSE, .Object = .Object@corpus, ...)
})


#' @rdname s_attributes-method
setMethod("s_attributes", "remote_partition", function(.Object, ...){
  ocpu_exec(fn = "s_attributes", server = .Object@server, .Object = as(.Object, "partition"), ...)
})

