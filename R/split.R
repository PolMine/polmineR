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
#' @rdname subcorpus-class
#' @inheritParams partition_bundle
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
  names(y@objects) <- sapply(y@objects, function(x) x@name)
  y
})


#' @examples
#' gparl <- corpus("GERMAPARLMINI")
#' b <- split(gparl, s_attribute = "date")
#' @export
#' @rdname corpus_class
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
  names(y@objects) <- sapply(y@objects, function(x) x@name)
  y
})



