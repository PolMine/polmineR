#' @include partitionBundle_class.R context_class.R
NULL

#' Generate a bundle of partitions
#' 
#' A partitionBundle object is a S4 class object. 
#' partitionBundle,character-method will create a bundle of partitions,
#' but not yet enriched.
#' 
#' @param .Object character string, a partition, or a list
#' @param sAttribute the s-attribute to vary
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
#' \dontrun{
#'   use("polmineR.sampleCorpus")
#'   bt2009 <- partition("PLPRBTTXT", text_year = "2009")
#'   pBundle <- partitionBundle(bt2009, sAttribute = "text_date", progress = TRUE, pAttribute = "word")
#'   dtm <- as.DocumentTermMatrix(pBundle, col = "count")
#'   summary(pBundle)
#'   btBundle <- partitionBundle("PLPRBTTXT", sAttribute = "text_date")
#' }
#' @seealso \code{\link{partition}} and \code{\link{bundle-class}}
setGeneric("partitionBundle", function(.Object, ...) standardGeneric("partitionBundle"))

#' @rdname partitionBundle-method
setMethod("partitionBundle", "partition", function(
  .Object, sAttribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE,
  ...
) {
  bundle <- new(
    "partitionBundle",
    corpus = .Object@corpus, sAttributesFixed = .Object@sAttributes,
    encoding = .Object@encoding, call = deparse(match.call())
  )
  if (is.null(values)){
    if (verbose) message('... getting values for s-attribute ', sAttribute)
    values <- sAttributes(.Object, sAttribute)
    if (verbose) message('... number of partitions to be generated: ', length(values))
  }
  bundle@objects <- blapply(
    lapply(setNames(values, rep(sAttribute, times = length(values))), function(x) setNames(x, sAttribute)),
    f = function(def, .Object, verbose = FALSE, ...) partition(.Object = .Object, def = def, verbose = FALSE, ...),
    .Object = .Object, progress = progress, verbose = if (progress) FALSE else verbose,  mc = mc,
    ...
  )
  names(bundle@objects) <- paste(as.corpusEnc(prefix, corpusEnc = bundle@encoding), values, sep='')
  bundle
})


#' @rdname partitionBundle-method
setMethod("partitionBundle", "character", function(
  .Object, sAttribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE, xml = "flat",
  ...
) {
  bundle <- new(
    "partitionBundle",
    corpus = .Object, encoding = RegistryFile$new(.Object)$getEncoding(),
    call = deparse(match.call())
  )
  strucs <- 0:(CQI$attribute_size(.Object, sAttribute, "s") - 1)
  names(strucs) <- CQI$struc2str(.Object, sAttribute, strucs)
  if (!is.null(values)) {
    valuesToKeep <- values[which(values %in% names(strucs))]
    strucs <- strucs[valuesToKeep]
  }

  values <- names(strucs)
  strucs <- unname(strucs)
  
  if (verbose) message("... getting matrix with regions for s-attribute: ", sAttribute)
  if (require("polmineR.Rcpp", quietly = TRUE)){
    cposMatrix <- polmineR.Rcpp::get_region_matrix(
      corpus = .Object, s_attribute = sAttribute, strucs = strucs,
      registry = Sys.getenv("CORPUS_REGISTRY")
      )
  } else {
    cposMatrix <- do.call(rbind, lapply(strucs, function(x) CQI$struc2cpos(.Object, sAttribute, x)))
  }
  
  cposList <- split(cposMatrix, f = values)
  cposList <- lapply(cposList, function(x) matrix(x, ncol = 2))
  
  if (verbose) message("... generating the partitions")
  .makeNewPartition <- function(i, corpus, encoding, sAttribute, cposList, xml, ...){
    newPartition <- new(
      "partition",
      corpus = corpus, encoding = encoding,
      stat = data.table(),
      cpos = cposList[[i]],
      size = sum(apply(cposList[[i]], 1, function(row) row[2] - row[1] + 1)),
      name = names(cposList)[i],
      sAttributes = setNames(list(names(cposList)[i]), sAttribute),
      sAttributeStrucs = sAttribute,
      xml = xml,
      strucs = CQI$cpos2struc(.Object, sAttribute, cposList[[i]][,1])
    )
  }
  bundle@objects <- blapply(
    setNames(as.list(1:length(cposList)), names(cposList)),
    f = .makeNewPartition,
    corpus = .Object, encoding = bundle@encoding, sAttribute = sAttribute, cposList, xml = xml,
    mc = mc, progress = progress, verbose = verbose, ...
    )
  bundle
})


setGeneric("as.partitionBundle", function(.Object, ...) standardGeneric("as.partitionBundle"))

#' @rdname partitionBundle-class
setMethod("as.partitionBundle", "list", function(.Object, ...){
  as(.Object, "bundle") # defined in bundle_class.R
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
