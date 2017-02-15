#' Initialize a partition.
#' 
#' Set up an object of the \code{partition} class. Frequency lists are computeted and kept 
#' in the stat-slot if pAttribute is not NULL.
#' 
#' The function sets up a partition based on a list of s-attributes with respective values.
#' The s-attributes defining the partition are a list, e.g. list(text_type="speech", text_year="2013").
#' The values of the list may contain regular expressions. To use regular expression syntax, set the 
#' parameter regex to \code{"TRUE"}. Regular expressions are passed into grep, i.e. the regex syntax
#' used in R needs to be used (double backlashes etc.).
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
#' @param .Object character-vector - the CWB-corpus to be used
#' @param def list consisting of a set of character vectors (see
#' details and examples)
#' @param name name of the new partition, defaults to "
#' @param encoding encoding of the corpus (typically "LATIN1 or "(UTF-8)), if NULL, the encoding provided in the registry file of the corpus (charset="...") will be used b
#' @param pAttribute the pAttribute(s) for which term frequencies shall be retrieved
#' @param meta a character vector
#' @param regex logical (defaults to FALSE), if TRUE, the s-attributes provided will be handeled as regular expressions; the length of the character vectors with s-attributes then needs to be 1
#' @param xml either 'flat' (default) or 'nested'
#' @param id2str whether to turn token ids to strings (set FALSE to minimize object.size / memory consumption)
#' @param type character vector (length 1) specifying the type of corpus / partition (e.g. "plpr")
#' @param mc whether to use multicore (for counting terms)
#' @param verbose logical, defaults to TRUE
#' @param slots character vector
#' @param ... parameters passed into the partition-method
#' @return An object of the S4 class 'partition'
#' @author Andreas Blaette
#' @examples
#' \dontrun{
#'    use(polmineR.sampleCorpus)
#'    spd <- partition(
#'      "PLPRBTTXT", text_party="SPD", text_type="speech"
#'      )
#'    kauder <- partition(
#'    "PLPRBTTXT", text_name="Volker Kauder", pAttribute="word"
#'    )
#'    merkel <- partition(
#'      "PLPRBTTXT", text_name=".*Merkel",
#'      pAttribute="word", regex=TRUE
#'      )
#'    sAttributes(merkel, "text_date")
#'    sAttributes(merkel, "text_name")
#'    merkel <- partition(
#'      "PLPRBTTXT", text_name="Angela Dorothea Merkel",
#'      text_date="2009-11-10", text_type="speech", pAttribute="word"
#'      )
#'    merkel <- subset(merkel, !word %in% punctuation)
#'    merkel <- subset(merkel, !word %in% tm::stopwords("de"))
#'    
#'    # a certain defined time segment
#'    if (require("chron")){
#'      firstDay <- "2009-10-28"
#'      lastDay <- "2009-11-11"
#'      days <- strftime(
#'        chron::seq.dates(
#'          from = strftime(firstDay, format="%m/%d/%Y"),
#'          to = strftime(lastDay, format="%m/%d/%Y"),
#'          by="days"),
#'        format="%Y-%m-%d"
#'        )
#'      period <- partition("PLPRBTTXT", text_date=days)
#'    }
#' }
#' @import methods
#' @exportMethod partition
#' @rdname partition
#' @include sAttributes2cpos_method.R
#' @aliases partition
setGeneric("partition", function(.Object, ...){standardGeneric("partition")})

#' @rdname partition
setMethod("partition", "character", function(
  .Object, def = NULL, name = "",
  encoding = NULL, pAttribute = NULL, meta = NULL, regex = FALSE, xml = "flat",
  id2str = TRUE, type = NULL, mc = FALSE, verbose = TRUE, ...
) {
  corpus <- .Object
  stopifnot(xml %in% c("nested", "flat"))
  if (!corpus %in% CQI$list_corpora()) stop("corpus not found (not installed / not in registry / a typo?)")
  if (length(list(...)) != 0 && is.null(def)) def <- list(...)
  if (!all(names(def) %in% sAttributes(.Object))) stop("not all sAttributes are available")
  if (verbose == TRUE) message('Setting up partition ', name)
  if (is.null(type)){
    parsedRegistry <- parseRegistry(.Object)
    if (!"type" %in% names(parsedRegistry)){
      partitionType <- "partition"
    } else {
      type <- parsedRegistry[["type"]]
      if (type %in% c("press", "plpr")){
        .verboseOutput(paste("type of the corpus is", type), verbose)
        partitionType <- paste(type, "Partition", sep="")
      } else {
        stop("partition type provided by registry is not valid")
      }
    }
  } else {
    if (type %in% c("press", "plpr")) partitionType <- paste(type, "Partition", sep="")
  }
  assign(
    "Partition",
    new(
      partitionType,
      stat = data.table(),
      call = deparse(match.call()),
      corpus = .Object,
      name = name,
      xml = xml
      )
    )  
  if(is.null(encoding)) {
    Partition@encoding <- getEncoding(Partition@corpus)  
  } else {
    Partition@encoding <- encoding
  }
  .verboseOutput(paste('get encoding:', Partition@encoding), verbose)
  Partition@sAttributes <- lapply(def, function(x) adjustEncoding(x, Partition@encoding))  

  .verboseOutput('get cpos and strucs', verbose)
  if(is.null(def)){
    parsedInfo <- parseInfoFile(.Object)
    if ("ANCHOR_ELEMENT" %in% names(parsedInfo)){
      def <- list()
      def[[parsedInfo["ANCHOR_ELEMENT"]]] <- ".*"
      regex <- TRUE
      Partition@sAttributeStrucs <- names(def)[length(def)]
      Partition <- sAttributes2cpos(Partition, xml, regex)
    } else {
      warning("no anchor element in corpus registry")
      Partition@cpos <- matrix(c(0, CQI$attribute_size(.Object, pAttributes(.Object)[1]) - 1), nrow = 1)
    }
  } else {
    Partition@sAttributeStrucs <- names(def)[length(def)]
    Partition <- sAttributes2cpos(Partition, xml, regex)  
  }
  if (!is.null(Partition)) {
    .verboseOutput('get partition size', verbose)
    Partition@size <- size(Partition)
    if (!is.null(pAttribute)) if (pAttribute[1] == FALSE) {pAttribute <- NULL}
    if (!is.null(pAttribute)) {
      Partition <- enrich(Partition, pAttribute = pAttribute, verbose = verbose, id2str = id2str, mc = mc)
      # Partition <- sort(Partition, "count")
    }
    if (!is.null(meta)) {
      .verboseOutput('get metadata', verbose)
      Partition@metadata <- meta(Partition, sAttributes=meta)
    }
  } else {
    message("... setting up the partition failed (returning NULL object)")
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
setMethod("partition", "partition", function(.Object, def = NULL, name = "", regex = FALSE, pAttribute = NULL, id2str = TRUE, type=NULL, verbose = TRUE, mc = FALSE, ...){
  if (length(list(...)) != 0 && is.null(def)) def <- list(...)
  newPartition <- new(
    class(.Object)[1],
    corpus=.Object@corpus, encoding=.Object@encoding, name=name, xml=.Object@xml,
    stat=data.table()
    )
  .verboseOutput(paste('Setting up partition', name), verbose)
  def <- lapply(def, function(x) adjustEncoding(x, .Object@encoding))  
  newPartition@sAttributes <- c(.Object@sAttributes, def)
  newPartition@sAttributeStrucs <- names(newPartition@sAttributes)[length(newPartition@sAttributes)]
  
  .verboseOutput('... getting cpos and strucs', verbose)
  
  if (.Object@xml == "flat") {
    str <- CQI$struc2str(.Object@corpus, names(def), .Object@strucs)    
  } else if (.Object@xml == "nested") {
    str <- CQI$struc2str(
      .Object@corpus, names(def),
      CQI$cpos2struc(.Object@corpus, names(def), .Object@cpos[,1])
      )
  }
  Encoding(str) <- newPartition@encoding
  if (regex == FALSE) {
    hits <- which(str %in% def[[1]])
  } else if (regex == TRUE) {
    hits <- grep(def[[1]], str)
  }
  newCpos <- .Object@cpos[hits,]
  if (class(newCpos) == "matrix"){
    newPartition@cpos <- newCpos
  } else if (class(newCpos) == "integer") {
    newPartition@cpos <- matrix(newCpos, ncol=2, byrow=TRUE)     
  }
  newPartition@strucs <- .Object@strucs[hits]
  if (length(.Object@metadata) == 2) {
    .verboseOutput('... remake metadata', verbose)
    newPartition@metadata <- .Object@metadata[hits,]
  }
  
  newPartition@size <- size(newPartition)
  if (length(pAttribute)>0) {
    newPartition@stat <- count(.Object = newPartition, pAttribute = pAttribute, id2str = id2str, mc = mc)
    newPartition@pAttribute <- pAttribute
  }
  newPartition
})

