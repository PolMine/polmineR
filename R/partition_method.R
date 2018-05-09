#' Initialize a partition.
#' 
#' Create a subcorpus stored in an object of the \code{partition} class.
#' Counts are performed for the p-attribute defined by the parameter \code{pAttribute}.
#' 
#' The function sets up a partition (subcorpus) based on a list of s-attributes with respective values.
#' 
#' The s-attributes defining the partition can be passed in as a list, e.g. list(interjection="speech",
#' year="2013"), or - for convencience - directly.
#' 
#' The values defining the partition may contain regular expressions. To use regular expression syntax, set the 
#' parameter regex to \code{"TRUE"}. Regular expressions are passed into grep, i.e. the regex syntax
#' used in R needs to be used (double backlashes etc.). If regular expressions are used, the length
#' of the character vector needs to be 1. If regex is \code{"FALSE"}, the length of the character
#' vectors can be > 1, matching s-attributes are identifies with the operator \code{in}.
#' 
#' The XML imported into the CWB may be "flat" or "nested". This needs to be indicated with the
#' parameter \code{xml} (default is "flat"). If you generate a partition based on a 
#' flat XML structure, some performance gain may be achieved when ordering the sAttributes
#' with decreasingly restrictive conditions. If you have a nested XML, it is mandatory that the
#' order of the sAttributes provided reflects the hierarchy of the XML: The top-level elements
#' need to be positioned at the beginning of the list with the s-attributes, the the most restrictive
#' elements at the end.
#' 
#' If pAttribute is not NULL, a count of tokens in the corpus will be performed and kept in the
#' \code{stat}-slot of the partition-object. The length of the pAttribute character vector may be 1
#' or more. If two or more p-attributes are provided, The occurrence of combinations will be counted.
#' A typical scenario is to combine the p-attributes "word" or "lemma" and "pos".
#' 
#' @param .Object character-vector - the CWB-corpus to be used
#' @param def list consisting of a set of character vectors (see
#' details and examples)
#' @param name name of the new partition, defaults to "
#' @param encoding encoding of the corpus (typically "LATIN1 or "(UTF-8)), if NULL, the encoding provided in the registry file of the corpus (charset="...") will be used b
#' @param pAttribute the pAttribute(s) for which term frequencies shall be retrieved
#' @param regex logical (defaults to FALSE)
#' @param xml either 'flat' (default) or 'nested'
#' @param decode whether to turn token ids to strings (set FALSE to minimize object.size / memory consumption)
#' @param type character vector (length 1) specifying the type of corpus / partition (e.g. "plpr")
#' @param mc whether to use multicore (for counting terms)
#' @param verbose logical, defaults to TRUE
#' @param slots character vector
#' @param ... parameters passed into the partition-method
#' @return An object of the S4 class 'partition'
#' @author Andreas Blaette
#' @seealso To learn about the methods available for objects of the class partition, see
#' \code{\link{partition_class}},
#' @examples
#' use("polmineR")
#' spd <- partition("GERMAPARLMINI", party = "SPD", interjection = "speech")
#' kauder <- partition("GERMAPARLMINI", speaker = "Volker Kauder", pAttribute = "word")
#' merkel <- partition("GERMAPARLMINI", speaker = ".*Merkel", pAttribute = "word", regex = TRUE)
#' sAttributes(merkel, "date")
#' sAttributes(merkel, "speaker")
#' merkel <- partition(
#'   "GERMAPARLMINI", speaker = "Angela Dorothea Merkel",
#'   date = "2009-11-10", interjection = "speech", pAttribute = "word"
#'   )
#' merkel <- subset(merkel, !word %in% punctuation)
#' merkel <- subset(merkel, !word %in% tm::stopwords("de"))
#'    
#' # a certain defined time segment
#' days <- seq(
#'   from = as.Date("2009-10-28"),
#'   to = as.Date("2009-11-11"),
#'   by = "1 day"
#' )
#' period <- partition("GERMAPARLMINI", date = days)
#' @import methods
#' @exportMethod partition
#' @rdname partition
#' @include sAttributes2cpos_method.R
#' @aliases partition
setGeneric("partition", function(.Object, ...){standardGeneric("partition")})

.getPartitionType <- function(corpus, type, verbose){
  if (is.null(type)){
    corpusProperties <- registry_get_properties(corpus)
    if (!"type" %in% names(corpusProperties)){
      partitionType <- "partition"
    } else {
      type <- corpusProperties[["type"]]
      if (type %in% c("press", "plpr")){
        .message("type of the corpus is", type, verbose = verbose)
        partitionType <- paste(type, "Partition", sep = "")
      } else {
        stop("partition type provided by registry is not valid")
      }
    }
  } else {
    if (type %in% c("press", "plpr")) partitionType <- paste(type, "Partition", sep = "")
  }
  partitionType
}



#' @rdname partition
setMethod("partition", "character", function(
  .Object, def = NULL, name = "",
  encoding = NULL, pAttribute = NULL, regex = FALSE, xml = "flat",
  decode = TRUE, type = NULL, mc = FALSE, verbose = TRUE, ...
) {
  corpus <- .Object
  stopifnot(xml %in% c("nested", "flat"))
  if (!corpus %in% CQI$list_corpora()) stop("corpus not found (not installed / not in registry / a typo?)")
  if (length(list(...)) != 0 && is.null(def)) def <- list(...)
  if (!all(names(def) %in% sAttributes(.Object))) stop("not all sAttributes are available")
  .message('Setting up partition ', name, verbose = verbose)
  partitionType <- .getPartitionType(corpus = .Object, type = type, verbose = verbose)
  assign(
    "Partition",
    new(
      partitionType,
      stat = data.table(), call = deparse(match.call()),
      corpus = .Object, name = name, xml = xml
      )
    )  
  
  Partition@encoding <- if (is.null(encoding)) getEncoding(Partition@corpus) else encoding
  .message('get encoding:', Partition@encoding, verbose = verbose)
  Partition@sAttributes <- lapply(def, function(x) as.corpusEnc(x, corpusEnc = Partition@encoding))

  .message('get cpos and strucs', verbose = verbose)
  if (is.null(def)){
    stop("no sAttributes provided to define partition")
  } else {
    Partition@sAttributeStrucs <- names(def)[length(def)]
    Partition <- sAttributes2cpos(Partition, xml, regex)  
  }
  if (!is.null(Partition)) {
    # get partition size
    Partition@size <- size(Partition)
    if (!is.null(pAttribute)) if (pAttribute[1] == FALSE) {pAttribute <- NULL}
    if (!is.null(pAttribute)) {
      Partition <- enrich(Partition, pAttribute = pAttribute, verbose = verbose, decode = decode, mc = mc)
    }
  } else {
    warning("... setting up the partition failed (returning NULL object)")
  }
  Partition
})


#' @rdname partition
setMethod("partition", "list", function(.Object, ...) {
  stopifnot(getOption("polmineR.corpus") %in% CQI$list_corpora())
  partition(.Object=getOption("polmineR.corpus"), def=.Object, ...)
})

#' @rdname partition
setMethod("partition", "environment", function(.Object, slots = c("name", "corpus", "size", "pAttribute")){
  partitionObjects <- getObjects(class = "partition", envir = .Object)
  if (length(slots) > 0){
    retval <- data.frame(
      c(
        list(object = partitionObjects),
        lapply(
          setNames(slots, slots),
          function(x) sapply(
            partitionObjects,
            function(y){
              value <- slot(get(y, envir = .Object), x)
              if (length(value) == 0){
                return(NA)
              } else {
                return(value)
              }
            }
            
          )
        )
      ),
      stringsAsFactors = FALSE
    )
    return(retval)
  } else {
    return(partitionObjects)
  }
})


#' @rdname partition
setMethod("partition", "partition", function(.Object, def = NULL, name = "", regex = FALSE, pAttribute = NULL, decode = TRUE, xml = NULL, verbose = TRUE, mc = FALSE, ...){
  if (length(list(...)) != 0 && is.null(def)) def <- list(...)
  if (!all(names(def) %in% sAttributes(.Object))) stop("some or all s-attributes provided are not available")
  if (length(def) > 1) stop("only one s-attribute allowed")
  if (!is.null(xml)) stopifnot(xml %in% c("flat", "nested"))
  
  newPartition <- new(
    class(.Object)[1], corpus = .Object@corpus, encoding = .Object@encoding, name = name,
    xml = if (is.null(xml)) .Object@xml else xml,
    stat = data.table()
    )
  .message('Setting up partition', name, verbose = verbose)
  def <- lapply(def, function(x) as.corpusEnc(x, corpusEnc = .Object@encoding))  
  newPartition@sAttributes <- c(.Object@sAttributes, def)
  newPartition@sAttributeStrucs <- names(def)[1]
  
  .message('getting cpos and strucs', verbose = verbose)
  
  if (.Object@xml == "flat") {
    sAttrValues <- CQI$struc2str(.Object@corpus, names(def), .Object@strucs)
    Encoding(sAttrValues) <- newPartition@encoding
    hits <- if (regex) grep(def[[1]], sAttrValues) else which(sAttrValues %in% def[[1]])
    newCposMatrix <- .Object@cpos[hits,]
    newPartition@cpos <- switch(
      class(newCposMatrix),
      "matrix" = newCposMatrix,
      "integer"= matrix(newCposMatrix, ncol = 2, byrow = TRUE)
    )
    newPartition@strucs <- .Object@strucs[hits]
  } else if (.Object@xml == "nested") {
    cposVec <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
    newStrucs <- CQI$cpos2struc(.Object@corpus, names(def)[1], cposVec)
    sAttrValues <- CQI$struc2str(.Object@corpus, names(def), newStrucs)
    Encoding(sAttrValues) <- .Object@encoding
    hits <- if (regex) grep(def[[1]], sAttrValues) else which(sAttrValues %in% def[[1]])
    newPartition@strucs <- unique(newStrucs[hits])
    if (requireNamespace("RcppCWB", quietly = TRUE)){
      newPartition@cpos <- RcppCWB::get_region_matrix(
        corpus = .Object@corpus, s_attribute = names(def),
        registry = Sys.getenv("CORPUS_REGISTRY"), strucs = newPartition@strucs
      )
    } else {
      newPartition@cpos <- matrix(
        unlist(lapply(
          newPartition@strucs,
          function(x) CQI$struc2cpos(.Object@corpus, names(def), x))
          ),
        byrow = TRUE, ncol = 2
      )
    }
  }
  newPartition@size <- size(newPartition)
  if (length(pAttribute) > 0) {
    newPartition@stat <- count(.Object = newPartition, pAttribute = pAttribute, decode = decode, mc = mc)
    newPartition@pAttribute <- pAttribute
  }
  newPartition
})

#' @rdname partition
#' @importFrom data.table copy
setMethod("partition", "Corpus", function(
  .Object, def = NULL, name = "",
  encoding = NULL, regex = FALSE, xml = "flat",
  type = NULL, verbose = TRUE, ...
) {
  
  # some checks to start with
  if (xml == "nested") stop("applying the partition on a Corpus object is possible only for flat XML")
  if (length(list(...)) != 0 && is.null(def)) def <- list(...)
  if (is.null(def)) stop("no sAttributes provided to define partition")
  if (!all(names(def) %in% colnames(.Object$sAttributes)))
    stop("at least one s-attribute is not present in data.table in slot sAttribute of .Object")
  
  encoding <- if (is.null(encoding)) getEncoding(.Object$corpus) else encoding
  .message('encoding of the corpus is:', encoding, verbose = verbose)
  
  .message('initialize partition ', name, verbose = verbose)
  partitionType <- .getPartitionType(corpus = .Object$corpus, type = type, verbose = verbose)
  assign(
    "y",
    new(
      partitionType,
      stat = data.table(), corpus = .Object$corpus, name = name, xml = xml,
      encoding = encoding,
      sAttributes = lapply(def, function(x) as.corpusEnc(x, corpusEnc = encoding))
    )
  ) 
  def <- y@sAttributes

  .message('get cpos and strucs', verbose = verbose)
  dt <- copy(.Object$sAttributes)
  for (sAttr in names(def)){
    if (regex){
      dt <- dt[unique(unlist(lapply(def[[sAttr]], function(x) grep(x, dt[[sAttr]]))))]
    } else {
      dt <- dt[which(dt[[sAttr]] %in% def[[sAttr]])]
    }
  }
  y@sAttributeStrucs <- names(def)[length(def)]
  y@strucs <- dt[["struc"]]
  y@cpos <- .Object$cpos[dt[["struc"]] + 1,]
  
  if (nrow(y@cpos) == 0){
    warning("... setting up the partition failed (returning NULL object)")
    return( NULL )
  }
  
  # get partition size
  y@size <- size(y)
  y
})


