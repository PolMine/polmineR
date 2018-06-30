#' @include partition.R bundle.R S4classes.R
NULL


#' @rdname partitionBundle-class
setMethod("show", "partitionBundle", function (object) {
  cat('** PartitionBundle object: **\n')
  cat(sprintf('%-25s', 'Number of partitions:'), length(object@objects), '\n')
  # same code as in show-method for partition
  sFix <- unlist(lapply(
    names(object@sAttributesFixed),
    function(x) paste(x, "=", paste(object@sAttributesFixed[[x]], collapse="/"))
  ))
  cat(sprintf("%-25s", "s-attributes Fixed:"), sFix[1], '\n')
  if (length(sFix)>1) {for (i in length(sFix)){cat(sprintf("%-25s", " "), sFix[i], '\n')}}
  cat("\n")
})

#' @rdname partitionBundle-class
setMethod("summary", "partitionBundle", function (object) {
  summary <- data.frame(
    partition=names(object@objects),
    token=unlist(lapply(object@objects, function(x) x@size)),
    stringsAsFactors=FALSE
  )
  pAttr <- unique(unlist(lapply(object@objects, function(x) x@pAttribute)))
  if (length(pAttr) == 1){
    raw <- lapply(pAttr, function(x) unlist(lapply(object@objects, function(y) nrow(y@stat))))
    raw <- do.call(data.frame, raw)
    colnames(raw) <- paste("unique_", pAttr, sep="")
    summary <- data.frame(summary, raw, stringsAsFactors=FALSE)
  }
  rownames(summary) <- c(1:nrow(summary))
  summary
})




#' Merge the partitions in a bundle into one partition
#' 
#' The partitions in a bundle object will be merged into one new partition
#' 
#' The function aggregates several partitions into one partition. The
#' prerequisite for this function to work properly is that there are no
#' overlaps of the different partitions that are to be summarized.
#' Encodings and the root node need to be identical, too.
#' 
#' @param x a bundle object
#' @param name the name for the new partition
#' @return An object of the class 'partition. See partition for the
#' details on the class.
#' @author Andreas Blaette
#' @exportMethod merge
#' @noRd
setMethod("merge", "partitionBundle", function(x, name = "", verbose = TRUE){
  y <- new("partition")
  .message('number of partitions to be merged: ', length(x@objects), verbose = verbose)
  y@corpus <- unique(vapply(x@objects, FUN.VALUE = "characer", function(p) p@corpus))
  if (length(y@corpus) >  1) warning("WARNING: This function will not work correctly, as the bundle comprises different corpora")
  y@xml <- unique(vapply(x@objects, function(p) p@xml, FUN.VALUE = "character"))
  y@encoding <- unique(vapply(x@objects, function(p) p@encoding, FUN.VALUE = "character"))
  y@sAttributeStrucs <- unique(vapply(x@objects, function(p) p@sAttributeStrucs, FUN.VALUE="character"))
  .message('merging the struc vectors', verbose = verbose)
  for (name in names(x@objects)) y@strucs <- union(y@strucs, x@objects[[name]]@strucs)
  .message('generating corpus positions', verbose = verbose)
  cpos <- data.matrix(t(data.frame(lapply(
    y@strucs,
    function(s) CQI$struc2cpos(y@corpus, y@sAttributeStrucs, s) )
  )))
  rownames(cpos) <- NULL
  y@cpos <- cpos
  y@size <- size(y)
  y@explanation = c(paste("this partition is a merger of the partitions", paste(names(x@objects), collapse=', ')))
  y@name <- name
  y
})



#' @exportMethod [
#' @rdname partitionBundle-class
setMethod('[', 'partitionBundle', function(x,i){
  a <- unname(unlist(lapply(x@objects, function(y) y@stat[i,2])))
  sizes <- unlist(lapply(x@objects, function(y) y@size))
  dist <- data.frame(
    partition=names(x@objects),
    count=a,
    freq=round(a/sizes*100000,2),
    row.names=c(1:length(x@objects))
  )
  dist
}
)


#' @exportMethod barplot
#' @rdname partitionBundle-class
setMethod("barplot", "partitionBundle", function(height, ...){
  tab <- summary(height)
  tab <- tab[order(tab[, "token"], decreasing=TRUE),]
  barplot(tab$token, names.arg=tab$partition, ...)
})


#' @include partitionBundle.R context.R
NULL

#' Generate a bundle of partitions
#' 
#' A partitionBundle object is a S4 class object. 
#' partitionBundle,character-method will create a bundle of partitions,
#' but not yet enriched.
#' 
#' @param .Object character string, a partition, or a list
#' @param s_attribute the s-attribute to vary
#' @param values values the s-attribute provided shall assume
#' @param prefix a character vector that will be attached as a prefix to partition names
#' @param progress logical, whether to show progress bar
#' @param mc logical, whether to use multicore parallelization
#' @param xml logical
#' @param verbose logical, whether to provide progress information
#' @param ... parameters to be passed into partition-method (see respective documentation)
#' @return S4 class 'partitionBundle', with list of partition objects in slot 'objects'
#' @export partitionBundle
#' @author Andreas Blaette
#' @name partitionBundle
#' @docType methods
#' @rdname partitionBundle-method
#' @examples
#' use("polmineR")
#' bt2009 <- partition("GERMAPARLMINI", date = "2009-.*", regex = TRUE)
#' pBundle <- partitionBundle(bt2009, s_attribute = "date", progress = TRUE, pAttribute = "word")
#' dtm <- as.DocumentTermMatrix(pBundle, col = "count")
#' summary(pBundle)
#' btBundle <- partitionBundle("GERMAPARLMINI", s_attribute = "date")
#' @seealso \code{\link{partition}} and \code{\link{bundle}}
setGeneric("partitionBundle", function(.Object, ...) standardGeneric("partitionBundle"))

#' @rdname partitionBundle-method
setMethod("partitionBundle", "partition", function(
  .Object, s_attribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE,
  ...
) {
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  bundle <- new(
    "partitionBundle",
    corpus = .Object@corpus, sAttributesFixed = .Object@sAttributes,
    encoding = .Object@encoding, call = deparse(match.call())
  )
  if (is.null(values)){
    .message('getting values for s-attribute ', s_attribute, verbose = verbose)
    values <- s_attributes(.Object, s_attribute)
    .message('number of partitions to be generated: ', length(values), verbose = verbose)
  }
  bundle@objects <- blapply(
    lapply(setNames(values, rep(s_attribute, times = length(values))), function(x) setNames(x, s_attribute)),
    f = function(def, .Object, verbose = FALSE, ...) partition(.Object = .Object, def = def, verbose = FALSE, ...),
    .Object = .Object, progress = progress, verbose = if (progress) FALSE else verbose,  mc = mc,
    ...
  )
  names(bundle@objects) <- paste(as.corpusEnc(prefix, corpusEnc = bundle@encoding), values, sep = "")
  for (i in 1L:length(bundle@objects)) bundle@objects[[i]]@name <- names(bundle@objects)[[i]]
  bundle
})


#' @rdname partitionBundle-method
setMethod("partitionBundle", "character", function(
  .Object, s_attribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE, xml = "flat",
  ...
) {
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  bundle <- new(
    Class = "partitionBundle",
    corpus = .Object,
    encoding = registry_get_encoding(.Object)
  )
  strucs <- 0L:(CQI$attribute_size(.Object, s_attribute, "s") - 1L)
  names(strucs) <- CQI$struc2str(.Object, s_attribute, strucs)
  if (!is.null(values)) {
    valuesToKeep <- values[which(values %in% names(strucs))]
    strucs <- strucs[valuesToKeep]
  }
  
  values <- names(strucs)
  Encoding(values) <- bundle@encoding
  strucs <- unname(strucs)
  
  .message("getting matrix with regions for s-attribute: ", s_attribute, verbose = verbose)
  cposMatrix <- RcppCWB::get_region_matrix(
    corpus = .Object, s_attribute = s_attribute, strucs = strucs,
    registry = Sys.getenv("CORPUS_REGISTRY")
  )
  
  cposList <- split(cposMatrix, f = values)
  cposList <- lapply(cposList, function(x) matrix(x, ncol = 2))
  
  .message("generating the partitions", verbose = verbose)
  .makeNewPartition <- function(i, corpus, encoding, s_attribute, cposList, xml, ...){
    newPartition <- new(
      "partition",
      corpus = corpus, encoding = encoding,
      stat = data.table(),
      cpos = cposList[[i]],
      size = sum(apply(cposList[[i]], 1, function(row) row[2] - row[1] + 1L)),
      name = names(cposList)[i],
      sAttributes = setNames(list(names(cposList)[i]), s_attribute),
      sAttributeStrucs = s_attribute,
      xml = xml,
      strucs = CQI$cpos2struc(.Object, s_attribute, cposList[[i]][,1])
    )
  }
  bundle@objects <- blapply(
    setNames(as.list(1L:length(cposList)), names(cposList)),
    f = .makeNewPartition,
    corpus = .Object, encoding = bundle@encoding, s_attribute = s_attribute, cposList, xml = xml,
    mc = mc, progress = progress, verbose = verbose, ...
  )
  bundle
})


setGeneric("as.partitionBundle", function(.Object, ...) standardGeneric("as.partitionBundle"))

#' @rdname partitionBundle-class
setMethod("as.partitionBundle", "list", function(.Object, ...){
  as(.Object, "bundle") # defined in bundle.R
})

#' @exportMethod as.partitionBundle
#' @rdname partitionBundle-method
setMethod("partitionBundle", "context", function(.Object, mc = getOption("polmineR.mc"), verbose = FALSE, progress = TRUE){
  newPartitionBundle <- new(
    "partitionBundle",
    corpus = .Object@corpus, encoding = .Object@encoding,
    explanation = "this partitionBundle is derived from a context object"
  )
  .makeNewPartition <- function(cpos, contextObject, ...){
    newPartition <- new(
      "partition",
      corpus = contextObject@corpus,
      encoding = contextObject@encoding,
      cpos = matrix(c(cpos[["left"]][1], cpos[["right"]][length(cpos[["right"]])]), ncol=2),
      stat = data.table()
    )
    newPartition <- enrich(newPartition, size=TRUE, pAttribute=contextObject@pAttribute, verbose=verbose)
    newPartition@strucs <- c(
      CQI$cpos2struc(contextObject@corpus, contextObject@sAttribute, newPartition@cpos[1,1])
      :
        CQI$cpos2struc(contextObject@corpus, contextObject@sAttribute, newPartition@cpos[1,2])
    )
    newPartition
  }
  newPartitionBundle@objects <- blapply(
    .Object@cpos, f = .makeNewPartition,
    contextObject = .Object, mc = mc, verbose = verbose, progress = progress
  )
  return(newPartitionBundle)
})

#' @rdname partitionBundle-class
setMethod("partitionBundle", "environment", function(.Object) getObjects(class = "partitionBundle", envir = .Object))


