#' @details The \code{split}-method will split a partition object into a
#' \code{partition_bundle} if gap between strucs exceeds a minimum number of
#' tokens specified by \code{gap}. Relevant to split up a plenary protocol# into
#' speeches. Note: To speed things up, the returned partitions will not include
#' frequency lists. The lists can be prepared by applying \code{enrich} on the
#' \code{partition_bundle} object that is returned.
#' @param x A \code{partition} object.
#' @param gap An integer value specifying the minimum gap between regions for
#'   performing the split.
#' @rdname partition_class
#' @exportMethod split
#' @docType methods
#' @examples
#' p <- partition("GERMAPARLMINI", date = "2009-11-11", speaker = "Norbert Lammert")
#' name(p) <- "Norbert Lammert"
#' pb <- split(p, gap = 500L)
#' summary(pb)
setMethod("split", "partition", function(x, gap, ...){
  if (nrow(x@cpos) > 1L){
    distance <- x@cpos[,1][2L:nrow(x@cpos)] - x@cpos[,2][1L:(nrow(x@cpos) - 1L)]
    beginning <- c(TRUE, ifelse(distance > gap, TRUE, FALSE))
    no <- cumsum(beginning)
    struc_list <- split(x@strucs, no)
    cpos_list <- split(x@cpos, no)
    y_list <- lapply(
      seq_along(struc_list),
      function(i) {
        p <- new(
          class(x)[1],
          strucs = struc_list[[i]],
          cpos = matrix(data = cpos_list[[i]], byrow = FALSE, ncol = 2L),
          corpus = x@corpus,
          encoding = x@encoding,
          s_attributes = x@s_attributes,
          xml = x@xml, s_attribute_strucs = x@s_attribute_strucs,
          explanation = "partition results from split, s-attributes do not necessarily define partition",
          name = paste(x@name, i, collapse = "_", sep = ""),
          stat = data.table()
        )
        p@size <- size(p)
        p
      })
  } else {
    x@name <- paste(x@name, 1, collapse = "_", sep = "")
    y_list <- list(x)
  }
  names(y_list) <- unlist(lapply(y_list, function(y) y@name))
  as.bundle(y_list)
})



#' @examples
#' sc <- subset("GERMAPARLMINI", date == "2009-11-11")
#' b <- split(sc, s_attribute = "speaker")
#' 
#' p <- partition("GERMAPARLMINI", date = "2009-11-11")
#' y <- partition_bundle(p, s_attribute = "speaker")
#' @export
#' @rdname subcorpus_bundle
#' @inheritParams partition_bundle
#' @param x A \code{corpus}, \code{subcorpus}, or \code{subcorpus_bundle}
#'   object.
setMethod("split", "subcorpus", function(
  x, s_attribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE,
  type = get_type(x), ...
) {
  
  y <- new(
    "subcorpus_bundle",
    corpus = x@corpus, s_attributes_fixed = x@s_attributes, encoding = x@encoding
  )
  
  if (x@xml == "nested") stop("splitting not yet implemented for nested XML")
  
  strucs <- cl_cpos2struc(corpus = x@corpus, s_attribute = s_attribute, cpos = x@cpos[,1], registry = registry())
  strucs_values <- cl_struc2str(corpus = x@corpus, s_attribute = s_attribute, struc = strucs, registry = registry())
  strucs_values <- as.nativeEnc(strucs_values, from = x@encoding)
  
  cpos_list <- split(x@cpos, strucs_values)
  struc_list <- split(strucs, strucs_values)
  
  if (!is.null(values))
    for (i in rev(which(!names(cpos_list) %in% values))) cpos_list[[i]] <- NULL
  
  y@objects <- lapply(
    seq_along(cpos_list),
    function(i){
      m <- matrix(cpos_list[[i]], ncol = 2L, byrow = FALSE)
      new(
        class(x)[1],
        corpus = x@corpus,
        data_dir = x@data_dir,
        type = x@type,
        encoding = x@encoding,
        name = names(cpos_list)[[i]],
        cpos = m,
        strucs = struc_list[[i]],
        s_attribute_strucs = s_attribute,
        s_attributes = c(x@s_attributes, setNames(list(names(cpos_list)[[i]]), s_attribute)),
        xml = "flat",
        size = sum((m[,2] + 1L) - m[,1])
        
      )
    }
  )
  if (nchar(prefix) == 0L) names(y@objects) <- names(cpos_list) else names(y) <- paste(prefix, names(cpos_list), sep = "_")
  names(y@objects) <- sapply(y@objects, function(x) x@name)
  y
})


#' @examples
#' gparl <- corpus("GERMAPARLMINI")
#' b <- split(gparl, s_attribute = "date")
#' @export
#' @rdname subcorpus_bundle
#' @inheritParams partition_bundle
setMethod("split", "corpus", function(
  x, s_attribute, values = NULL, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE,
  type = get_type(x), ...
) {
  
  y <- new("subcorpus_bundle", corpus = x@corpus, encoding = x@encoding)

  struc_size <- cl_attribute_size(corpus = x@corpus, attribute = s_attribute, attribute_type = "s", registry = registry())
  strucs <- 0L:(struc_size - 1L)
  cpos_matrix <- get_region_matrix(corpus = x@corpus, s_attribute = s_attribute, strucs = strucs, registry = registry())
  strucs_values <- cl_struc2str(corpus = x@corpus, s_attribute = s_attribute, struc = strucs, registry = registry())
  strucs_values <- as.nativeEnc(strucs_values, from = x@encoding)
  
  
  cpos_list <- split(cpos_matrix, strucs_values)
  struc_list <- split(strucs, strucs_values)
  
  if (!is.null(values)){
    for (i in rev(which(!names(cpos_list) %in% values))) cpos_list[[i]] <- NULL
  }
  
  new_class <- if (length(x@type) == 0L) "subcorpus" else paste(x@type, "subcorpus", sep = "_")
  y@objects <- lapply(
    seq_along(cpos_list),
    function(i){
      m <- matrix(cpos_list[[i]], ncol = 2L, byrow = FALSE)
      new(
        new_class,
        corpus = x@corpus,
        data_dir = x@data_dir,
        type = x@type,
        encoding = x@encoding,
        name = names(cpos_list)[[i]],
        cpos = m,
        strucs = struc_list[[i]],
        s_attributes = setNames(list(names(cpos_list)[[i]]), s_attribute),
        s_attribute_strucs = s_attribute,
        xml = "flat",
        size = sum((m[,2] + 1L) - m[,1])
      )
    }
  )
  if (nchar(prefix) == 0L) names(y@objects) <- names(cpos_list) else names(y) <- paste(prefix, names(cpos_list), sep = "_")
  names(y@objects) <- sapply(y@objects, function(x) x@name)
  y
})


#' @details Applying the \code{split}-method to a \code{subcorpus_bundle}-object
#'   will iterate through the subcorpus, and apply \code{split} on each
#'   \code{subcorpus} object in the bundle, splitting it up by the s-attribute
#'   provided by the argument \code{s_attribute}. The return value is a
#'   \code{subcorpus_bundle}, the names of which will be the names of the
#'   incoming \code{partition_bundle} concatenated with the s-attribute values
#'   used for splitting. The argument \code{prefix} can be used to achieve a
#'   more descriptive name.
#' @examples
#' # split up objects in partition_bundle by using partition_bundle-method
#' use("polmineR")
#' y <- corpus("GERMAPARLMINI") %>%
#'   split(s_attribute = "date") %>%
#'   split(s_attribute = "speaker")
#' 
#' summary(y)
#' @rdname subcorpus_bundle
setMethod("split", "subcorpus_bundle", function(x, s_attribute, prefix = "", progress = TRUE, mc = getOption("polmineR.mc")){
  
  if (is.logical(mc)) mc <- if (isTRUE(mc)) as.integer(getOption("polmineR.cores")) else 1L
  mc <- as.integer(mc)
  stopifnot(length(mc) == 1L, !is.na(mc), is.integer(mc))
  
  .fn <- function(sc){
    y <- split(x = sc, s_attribute = s_attribute, verbose = FALSE, progress = FALSE)
    names(y) <- paste(name(sc), paste(prefix, names(y), sep = if (nchar(prefix) > 0) "_" else ""), sep = "_")
    y@objects
  }
  li <- if (progress) pblapply(x@objects, .fn, cl = mc) else lapply(x@objects, .fn)
  as(unlist(li), "bundle")
})


