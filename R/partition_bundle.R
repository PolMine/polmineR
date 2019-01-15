#' @include partition.R bundle.R S4classes.R
NULL


#' @rdname partition_bundle-class
setMethod("show", "partition_bundle", function (object) {
  cat('** partition_bundle object: **\n')
  cat(sprintf('%-25s', 'Number of partitions:'), length(object@objects), '\n')
  # same code as in show-method for partition
  sFix <- unlist(lapply(
    names(object@s_attributes_fixed),
    function(x) paste(x, "=", paste(object@s_attributes_fixed[[x]], collapse="/"))
  ))
  cat(sprintf("%-25s", "s-attributes Fixed:"), sFix[1], '\n')
  if (length(sFix) > 1) for (i in length(sFix)) cat(sprintf("%-25s", " "), sFix[i], '\n')
  cat("\n")
})

#' @rdname partition_bundle-class
setMethod("summary", "partition_bundle", function (object, progress = TRUE){
  if (progress){
    a <- lapply(object@objects, function(x) data.frame(summary(x), stringsAsFactors = FALSE))
  } else {
    a <- pblapply(object@objects, function(x) data.frame(summary(x), stringsAsFactors = FALSE))
  }
  
  y <- do.call(rbind, a)
  rownames(y) <- NULL
  y
})



#' @details The \code{merge}-method aggregates several partitions into one partition. The
#' prerequisite for this function to work properly is that there are no
#' overlaps of the different partitions that are to be summarized.
#' Encodings and the root node need to be identical, too.
#' @param name the name for the new partition
#' @return An object of the class 'partition. See partition for the
#' details on the class.
#' @exportMethod merge
#' @rdname partition_bundle-class
setMethod("merge", "partition_bundle", function(x, name = "", verbose = TRUE){
  y <- new("partition")
  .message('number of partitions to be merged: ', length(x@objects), verbose = verbose)
  y@corpus <- unique(vapply(x@objects, FUN.VALUE = "characer", function(p) p@corpus))
  if (length(y@corpus) >  1) warning("WARNING: This function will not work correctly, as the bundle comprises different corpora")
  y@xml <- unique(vapply(x@objects, function(p) p@xml, FUN.VALUE = "character"))
  y@encoding <- unique(vapply(x@objects, function(p) p@encoding, FUN.VALUE = "character"))
  y@s_attribute_strucs <- unique(vapply(x@objects, function(p) p@s_attribute_strucs, FUN.VALUE = "character"))
  .message('merging the struc vectors', verbose = verbose)
  for (name in names(x@objects)) y@strucs <- union(y@strucs, x@objects[[name]]@strucs)
  .message('generating corpus positions', verbose = verbose)
  cpos <- data.matrix(t(data.frame(lapply(
    y@strucs,
    function(s) CQI$struc2cpos(y@corpus, y@s_attribute_strucs, s) )
  )))
  rownames(cpos) <- NULL
  y@cpos <- cpos
  y@size <- size(y)
  y@explanation = c(paste("this partition is a merger of the partitions", paste(names(x@objects), collapse=', ')))
  y@name <- name
  y
})



#' @details Using brackets can be used to retrieve the count for a token from the 
#' \code{partition} objects in a \code{partition_bundle}.
#' @exportMethod [
#' @rdname partition_bundle-class
setMethod('[', 'partition_bundle', function(x,i){
  a <- unname(unlist(lapply(x@objects, function(y) y@stat[i,2])))
  sizes <- unlist(lapply(x@objects, function(y) y@size))
  dist <- data.frame(
    partition = names(x@objects),
    count = a,
    freq = round(a / sizes * 100000, 2),
    row.names = 1L:length(x@objects)
  )
  dist
}
)


#' @exportMethod barplot
#' @rdname partition_bundle-class
setMethod("barplot", "partition_bundle", function(height, ...){
  tab <- summary(height)
  tab <- tab[order(tab[, "token"], decreasing = TRUE),]
  barplot(tab$token, names.arg=tab$partition, ...)
})


#' @include partition_bundle.R context.R
NULL

#' Generate bundle of partitions.
#' 
#' Use \code{partition_bundle} to create a \code{partition_bundle} object, which
#' combines a set of \code{partition} objects.
#' 
#' @param .Object A \code{partition}, a length-one \code{character} vector supplying a CWB corpus, or a \code{partition_bundle}
#' @param s_attribute The s-attribute to vary
#' @param values Values the s-attribute provided shall assume.
#' @param prefix A character vector that will be attached as a prefix to partition names.
#' @param progress Logical, whether to show progress bar.
#' @param mc Logical, whether to use multicore parallelization.
#' @param xml logical
#' @param type The type of \code{partition} to generate.
#' @param verbose Logical, whether to provide progress information.
#' @param ... parameters to be passed into partition-method (see respective documentation)
#' @return S4 class \code{partition_bundle}, with list of partition objects in slot 'objects'
#' @export partition_bundle
#' @author Andreas Blaette
#' @name partition_bundle
#' @docType methods
#' @rdname partition_bundle-method
#' @examples
#' use("polmineR")
#' bt2009 <- partition("GERMAPARLMINI", date = "2009-.*", regex = TRUE)
#' pb <- partition_bundle(bt2009, s_attribute = "date", progress = TRUE, p_attribute = "word")
#' dtm <- as.DocumentTermMatrix(pb, col = "count")
#' summary(pb)
#' pb <- partition_bundle("GERMAPARLMINI", s_attribute = "date")
#' @seealso \code{\link{partition}} and \code{\link{bundle}}
setGeneric("partition_bundle", function(.Object, ...) standardGeneric("partition_bundle"))

#' @rdname partition_bundle-method
setMethod("partition_bundle", "partition", function(
  .Object, s_attribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE,
  type = get_type(.Object), ...
) {
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  bundle <- new(
    "partition_bundle",
    corpus = .Object@corpus, s_attributes_fixed = .Object@s_attributes,
    encoding = .Object@encoding, call = deparse(match.call())
  )
  if (is.null(values)){
    .message('getting values for s-attribute ', s_attribute, verbose = verbose)
    values <- s_attributes(.Object, s_attribute)
    .message('number of partitions to be generated: ', length(values), verbose = verbose)
  }
  bundle@objects <- blapply(
    lapply(setNames(values, rep(s_attribute, times = length(values))), function(x) setNames(x, s_attribute)),
    f = function(def, .Object, verbose = FALSE, type, ...) partition(.Object = .Object, def = def, type = type, verbose = FALSE, ...),
    .Object = .Object, progress = progress, verbose = if (progress) FALSE else verbose,  mc = mc, type = type,
    ...
  )
  names(bundle@objects) <- paste(as.corpusEnc(prefix, corpusEnc = bundle@encoding), values, sep = "")
  for (i in 1L:length(bundle@objects)) bundle@objects[[i]]@name <- names(bundle@objects)[[i]]
  bundle
})


#' @rdname partition_bundle-method
setMethod("partition_bundle", "character", function(
  .Object, s_attribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE, xml = "flat", type = get_type(.Object),
  ...
) {
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  bundle <- new(
    Class = "partition_bundle",
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
  .makeNewPartition <- function(i, corpus, encoding, s_attribute, cposList, xml, type, ...){
    newPartition <- new(
      Class = paste(c(type, "partition"), collapse = "_"),
      corpus = corpus, encoding = encoding,
      stat = data.table(),
      cpos = cposList[[i]],
      size = sum(apply(cposList[[i]], 1, function(row) row[2] - row[1] + 1L)),
      name = names(cposList)[i],
      s_attributes = setNames(list(names(cposList)[i]), s_attribute),
      s_attribute_strucs = s_attribute,
      xml = xml,
      strucs = CQI$cpos2struc(.Object, s_attribute, cposList[[i]][,1])
    )
  }
  bundle@objects <- blapply(
    setNames(as.list(1L:length(cposList)), names(cposList)),
    f = .makeNewPartition,
    corpus = .Object, encoding = bundle@encoding, s_attribute = s_attribute, cposList, xml = xml,
    mc = mc, progress = progress, verbose = verbose, type = type, ...
  )
  bundle
})


setGeneric("as.partition_bundle", function(.Object, ...) standardGeneric("as.partition_bundle"))

#' @rdname partition_bundle-class
setMethod("as.partition_bundle", "list", function(.Object, ...){
  as(.Object, "bundle") # defined in bundle.R
})


#' @param node A logical value, whether to include the node (i.e. query matches) in the region matrix
#' generated when creating a \code{partition} from a \code{context}-object.
#' @exportMethod as.partition_bundle
#' @rdname partition_bundle-method
setMethod("partition_bundle", "context", function(.Object, node = TRUE, progress = TRUE, mc = 1L){
  
  stopifnot(is.logical(node))
  
  DT <- copy(.Object@cpos)
  setkeyv(x = DT, cols = c("hit_no", "cpos"))
  if (!node) DT <- subset(DT, DT[["position"]] != 0)
  
  # First step, generate a list of data.tables with regions
  .cpos_left_right <- function(.SD) list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
  DT_list <- list(left = subset(DT, DT[["position"]] < 0), right = subset(DT, DT[["position"]] > 0))
  if (node) DT_list[["node"]] <- subset(DT, DT[["position"]] == 0)
  DT_regions <- rbindlist(lapply(DT_list, function(x) x[, .cpos_left_right(.SD), by = "hit_no"]))
  setorderv(DT_regions, cols = "hit_no")
  regions_list <- split(DT_regions, by = "hit_no")
  
  # Second, generate a list with data.table objects with counts
  CNT <- DT[, .N, by = c("hit_no", paste(.Object@p_attribute, "id", sep = "_"))]
  setnames(CNT, old = "N", new = "count")
  for (p_attr in .Object@p_attribute){
    CNT[[p_attr]] <- CQI$id2str(corpus = .Object@corpus, p_attribute = p_attr, id = CNT[[paste(p_attr, "id", sep = "_")]])
  }
  count_list <- split(CNT, by = "hit_no")
  
  .fn <- function(i){
    cpos_matrix <- as.matrix(regions_list[[i]][, c("cpos_left", "cpos_right")])
    new(
      "partition",
      corpus = .Object@corpus,
      encoding = .Object@encoding,
      cpos = cpos_matrix,
      size = sum(cpos_matrix[,2] - cpos_matrix[,1] + 1L),
      xml = .Object@partition@xml,
      p_attribute = .Object@p_attribute,
      stat = count_list[[i]][, "hit_no" := NULL]
    )
  }
  partition_objects <- if (progress) pblapply(1L:length(.Object), .fn, cl = mc) else lapply(1L:length(.Object), .fn)
  new(
    "partition_bundle",
    corpus = .Object@corpus,
    encoding = .Object@encoding,
    p_attribute = .Object@p_attribute,
    objects = partition_objects,
    explanation = "this partition_bundle is derived from a context object"
  )
})

#' @rdname partition_bundle-class
setMethod("partition_bundle", "environment", function(.Object) .get_objects(class = "partition_bundle", envir = .Object))


#' @details Applying the \code{partition_bundle}-method to a \code{partition_bundle}-object will iterate
#' through the \code{partition} objects in the \code{object}-slot in the \code{partition_bundle}, and apply
#' \code{partition_bundle} on each \code{partition}, splitting it up by the s-attribute provided by the 
#' argument \code{s_attribute}. The return value is a \code{partition_bundle}, the names of which will be
#' the names of the incoming \code{partition_bundle} concatenated with the s-attribute values used for splitting.
#' The argument \code{prefix} can be used to achieve a more descriptive name.
#' @examples
#' # split up objects in partition_bundle by using partition_bundle-method
#' use("polmineR")
#' pb <- partition_bundle("GERMAPARLMINI", s_attribute = "date")
#' pb2 <- partition_bundle(pb, s_attribute = "speaker", progress = FALSE)
#' 
#' summary(pb2)
#' @rdname partition_bundle-method
setMethod("partition_bundle", "partition_bundle", function(.Object, s_attribute, prefix = character(), progress = TRUE, mc = getOption("polmineR.mc")){
  
  if (is.logical(mc)) mc <- if (isTRUE(mc)) as.integer(getOption("polmineR.cores")) else 1L
  mc <- as.integer(mc)
  stopifnot(length(mc) == 1L, !is.na(mc), is.integer(mc))
  
  iterfun <- function(p){
    pb <- partition_bundle(p, s_attribute = s_attribute, verbose = FALSE, progress = FALSE)
    names(pb) <- paste(name(p), paste(prefix, names(pb), sep = if (length(prefix) > 0) "_" else ""), sep = "_")
    pb@objects
  }
  partition_list_nested <- if (progress) pblapply(.Object@objects, iterfun, cl = mc) else lapply(.Object@objects, iterfun)
  as.partition_bundle(unlist(partition_list_nested))
})
