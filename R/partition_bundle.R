#' @include partition.R bundle.R S4classes.R
NULL


#' @rdname partition_bundle-class
setMethod("show", "partition_bundle", function (object) {
  message('** partition_bundle object: **')
  message(sprintf('%-25s', 'Number of partitions:'), length(object@objects))
  # same code as in show-method for partition
  sFix <- unlist(lapply(
    names(object@s_attributes_fixed),
    function(x) paste(x, "=", paste(object@s_attributes_fixed[[x]], collapse="/"))
  ))
  message(sprintf("%-25s", "s-attributes fixed:"), sFix[1])
  if (length(sFix) > 1) for (i in length(sFix)) message(sprintf("%-25s\n"), sFix[i])
})

#' @rdname partition_bundle-class
setMethod("summary", "partition_bundle", function (object, progress = FALSE){
  .fn <- function(x) data.frame(summary(x), stringsAsFactors = FALSE)
  a <- if (!progress) lapply(object@objects, .fn) else pblapply(object@objects, .fn)
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
#' @examples 
#' 
#' # merge partition_bundle into one partition
#' gparl <- corpus("GERMAPARLMINI") %>%
#'   split(s_attribute = "date") %>% 
#'   merge()
setMethod("merge", "partition_bundle", function(x, name = "", verbose = FALSE){
  corpus_id <- get_corpus(x)
  if (length(corpus_id) >  1L){
    warning(
      "WARNING: Merging will not work correctly, ",
      "as the objects within the bundle are derived from different corpora."
    )
  }
  
  obj_type <- unique(unname(sapply(x@objects, class)))
  if (length(obj_type) > 1L) stop("Class of the objects within the bundle is not unique.")

  .message('number of objects to be merged: ', length(x@objects), verbose = verbose)
  
  s_attr <- unique(unname(unlist(lapply(x@objects, slot,  "s_attribute_strucs"))))
  strucs_combined <- unname(unlist(lapply(x@objects, slot,  "strucs")))
  if (any(table(strucs_combined) > 1L)) stop("The objects are not non-overlapping.")
  strucs_combined <- unique(strucs_combined)
  strucs_combined <- strucs_combined[order(strucs_combined)]

  y <- new(
    obj_type,
    corpus = corpus_id, xml = x[[1]]@xml, encoding = x[[1]]@encoding,
    s_attribute_strucs = s_attr, strucs = strucs_combined,
    name = name
  )
  y@cpos <- get_region_matrix(corpus = corpus_id, s_attribute = s_attr, strucs = strucs_combined, registry = registry())
  y@size <- size(y)
  y
})


#' @param name The name of the new \code{subcorpus} object.
#' @rdname subcorpus_bundle
setMethod("merge", "subcorpus_bundle", function(x, name = "", verbose = FALSE){
  y <- callNextMethod()
  y@type <- get_type(y@corpus)
  y@data_dir <- registry_get_home(corpus = y@corpus, registry = registry())
  y
})


#' @param ... Further \code{subcorpus} objects to be merged with \code{x} and \code{y}.
#' @param y A \code{subcorpus} to be merged with \code{x}.
#' @examples
#' 
#' # Merge multiple subcorpus objects
#' a <- corpus("GERMAPARLMINI") %>% subset(date == "2009-10-27")
#' b <- corpus("GERMAPARLMINI") %>% subset(date == "2009-10-28")
#' c <- corpus("GERMAPARLMINI") %>% subset(date == "2009-11-10")
#' y <- merge(a, b, c)
#' s_attributes(y, "date")
#' @rdname subcorpus_bundle
setMethod("merge", "subcorpus", function(x, y, ...){
  merge(as(c(list(x), c(y, list(...))), "bundle"), name = "", verbose = FALSE)
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
#' @examples
#' pb <- partition_bundle("REUTERS", s_attribute = "id")
#' barplot(pb, las = 2)
#' 
#' sc <- corpus("GERMAPARLMINI") %>%
#'   subset(date == "2009-11-10") %>%
#'   split(s_attribute = "speaker") %>%
#'   barplot(las = 2)
setMethod("barplot", "partition_bundle", function(height, ...){
  tab <- summary(height)
  tab <- tab[order(tab[["size"]], decreasing = TRUE),]
  barplot(tab[["size"]], names.arg = tab[["name"]], ...)
})


#' @include partition_bundle.R context.R
NULL

#' Generate bundle of partitions.
#' 
#' Use \code{partition_bundle} to create a \code{partition_bundle} object, which
#' combines a set of \code{partition} objects.
#' 
#' @param .Object A \code{partition}, a length-one \code{character} vector supplying a CWB corpus, or a \code{partition_bundle}
#' @param s_attribute The s-attribute to vary.
#' @param values Values the s-attribute provided shall assume.
#' @param prefix A character vector that will be attached as a prefix to partition names.
#' @param progress Logical, whether to show progress bar.
#' @param mc Logical, whether to use multicore parallelization.
#' @param xml A \code{logical} value.
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
#' pb <- partition_bundle(bt2009, s_attribute = "date", progress = TRUE)
#' pb <- enrich(pb, p_attribute = "word")
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
  split(
    x = as(.Object, "subcorpus"), s_attribute = s_attribute, values = values, prefix = prefix,
    mc = mc, verbose = verbose, progress = progress,
    type = type,
    ...
  )
})

#' @exportMethod partition_bundle
#' @rdname partition_bundle-method
setMethod("partition_bundle", "corpus", function(
  .Object, s_attribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE, xml = "flat", type = get_type(.Object),
  ...
){
  split(
    x = .Object,
    s_attribute = s_attribute,
    values = values,
    prefix = prefix,
    mc = mc, 
    verbose = verbose,
    progress = progress,
    xml = xml,
    type = type,
    ...
  )
})


#' @rdname partition_bundle-method
setMethod("partition_bundle", "character", function(
  .Object, s_attribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE, xml = "flat", type = get_type(.Object),
  ...
) {
  partition_bundle(
    .Object = corpus(.Object), s_attribute = s_attribute, values = values, prefix = prefix,
    mc = mc, verbose = verbose, progress = progress,
    xml = xml, type = type, ...
  )
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
  setkeyv(x = DT, cols = c("match_id", "cpos"))
  if (!node) DT <- subset(DT, DT[["position"]] != 0)
  
  # First step, generate a list of data.tables with regions
  .cpos_left_right <- function(.SD) list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
  DT_list <- list(left = subset(DT, DT[["position"]] < 0), right = subset(DT, DT[["position"]] > 0))
  if (node) DT_list[["node"]] <- subset(DT, DT[["position"]] == 0)
  DT_regions <- rbindlist(lapply(DT_list, function(x) x[, .cpos_left_right(.SD), by = "match_id"]))
  setorderv(DT_regions, cols = "match_id")
  regions_list <- split(DT_regions, by = "match_id")
  
  # Second, generate a list with data.table objects with counts
  CNT <- DT[, .N, by = c("match_id", paste(.Object@p_attribute, "id", sep = "_"))]
  setnames(CNT, old = "N", new = "count")
  for (p_attr in .Object@p_attribute){
    CNT[[p_attr]] <- cl_id2str(corpus = .Object@corpus, p_attribute = p_attr, id = CNT[[paste(p_attr, "id", sep = "_")]], registry = registry())
  }
  count_list <- split(CNT, by = "match_id")
  
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
      stat = count_list[[i]][, "match_id" := NULL]
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
