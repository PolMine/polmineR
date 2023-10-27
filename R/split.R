#' @details The `split()`-method will split a partition object into a
#'   `partition_bundle` if gap between strucs exceeds a minimum number of tokens
#'   specified by `gap`. Relevant to split up a plenary protocol into speeches.
#'   Note: To speed things up, the returned partitions will not include
#'   frequency lists. The lists can be prepared by applying `enrich` on the
#'   `partition_bundle` object that is returned.
#' @param x A `partition` object.
#' @param gap An integer value specifying the minimum gap between regions for
#'   performing the split.
#' @rdname partition_class
#' @exportMethod split
#' @docType methods
#' @examples
#' p <- partition(
#'   "GERMAPARLMINI",
#'   date = "2009-11-11",
#'   speaker = "Norbert Lammert"
#'  )
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
        p <- x
        p@strucs <- struc_list[[i]]
        p@cpos <- matrix(data = cpos_list[[i]], byrow = FALSE, ncol = 2L)
        p@name = paste(x@name, i, collapse = "_", sep = "_")
        p@stat = data.table()
        p@size <- size(p)
        p
      })
  } else {
    x@name <- paste(x@name, 1, collapse = "_", sep = "_")
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
#' @param values Either a `character` vector with values used for splitting, or 
#'   a `logical` value: If `TRUE`, changes of s-attribute values will be the
#'   basis for generating subcorpora. If `FALSE`, a new subcorpus is generated
#'   for every struc of the s-attribute. If missing (default), `TRUE`/`FALSE` is
#'   assigned depending on whether `s-attribute` has values, or not.
#' @importFrom RcppCWB s_attr_is_sibling s_attr_is_descendent
#' @importFrom cli col_cyan
#' @inheritParams partition_bundle
#' @param x A `corpus`, `subcorpus`, or `subcorpus_bundle`
#'   object.
setMethod("split", "subcorpus", function(
  x, s_attribute, values, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE,
  type = get_type(x)
) {
  
  stopifnot(is.character(s_attribute), length(s_attribute) == 1L)
  if (!s_attribute %in% s_attributes(x))
    stop(sprintf("s-attribute '%s' not available", s_attribute))

  if (missing(values))
    # prospectvely use RcppCWB::cl_struc_values()
    values <- s_attr_has_values(s_attribute = s_attribute, x = x)
  if (is.null(values)) values <- TRUE
  # result is reported, yet later with other info on s-attribute
  
  history <- rev(sapply(sys.calls(), function(x) deparse(x[[1]])))
  pb_call <- if (5L %in% which(history == "partition_bundle")) TRUE else FALSE
  retval_class <- if (isTRUE(pb_call)) "partition_bundle" else "subcorpus_bundle"
  y <- as(as(x, "corpus"), retval_class)
  if (verbose) cli_alert_info("bundle class: {col_cyan({retval_class})}")
  
  cl <- if (isTRUE(pb_call)) "partition" else "subcorpus"
  new_class <- if (length(x@type) == 0L || is.na(x@type)){
    cl
  } else {
    paste(x@type, cl, sep = "_") 
  }
  
  prototype <- as(x, new_class)
  if (verbose) cli_alert_info("objects in bundle: {col_cyan({new_class})}")

  y@s_attributes_fixed <- x@s_attributes
  
  is_sibling <- s_attr_is_sibling(
    x = x@s_attribute_strucs,
    y = s_attribute,
    corpus = x@corpus,
    registry = x@registry_dir
  )
  
  if (!is_sibling){
    is_descendent <- s_attr_is_descendent(
      x = s_attribute,
      y = x@s_attribute_strucs,
      corpus = x@corpus,
      registry = x@registry_dir
    )
    relation <- if (is_descendent) "descendent" else "ancestor"
  } else {
    relation <- "sibling"
  }
  if (verbose){
    cli_alert_info(
      paste0(
        "s-attribute for splitting ({.val {s_attribute}}) is {relation} ",
        "of s-attribute {.val {x@s_attribute_strucs}}"
      )
    )
    
    # report here, together with further info on s-attribute 
    cli_alert_info(
      's-attribute {.val {s_attribute}} has values: {col_cyan({if (isFALSE(values)) "no" else "yes"})}'
    )
  }

  if (verbose) cli_progress_step("get list of regions")

  if (relation %in% c("sibling", "ancestor")){
    strucs <- cpos2struc(x, s_attr = s_attribute, cpos = x@cpos[,1])
    if (isTRUE(values)){
      strucs_values <- struc2str(x, s_attr = s_attribute, struc = strucs)
    } else if (isFALSE(values)){
      if (verbose) cli_alert_info("split by change of strucs")
      strucs_values <- strucs
    } else {
      stop("not implemented")
    }
    cpos_list <- split(x@cpos, strucs_values)
    
    if (relation == "sibling"){
      struc_list <- split(strucs, strucs_values)
      s_attr_strucs <- s_attribute
    } else if (relation == "ancestor"){
      struc_list <- split(x@strucs, strucs_values) # different from sibling
      s_attr_strucs <- x@s_attribute_strucs
    }
  } else if (relation == "descendent"){
    # cpos <- ranges_to_cpos(x@cpos)
    struc_matrix <- RcppCWB::region_matrix_to_struc_matrix(
      corpus = x@corpus,
      s_attribute = s_attribute,
      region_matrix = x@cpos,
      registry = x@registry_dir
    )
    strucs <- RcppCWB::ranges_to_cpos(struc_matrix)
    # strucs <- unique(cpos2struc(x, cpos = cpos, s_attr = s_attribute))
    regions <- get_region_matrix(
      corpus = x@corpus,
      s_attribute = s_attribute,
      strucs = strucs,
      registry = x@registry_dir
    )
    if (isTRUE(values)){
      strucs_values <- struc2str(x, s_attr = s_attribute, struc = strucs)
    } else if (isFALSE(values)){
      if (verbose) cli_alert_info("split by change of strucs")
      strucs_values <- strucs
    }
    cpos_list <- split(regions, strucs_values)
    struc_list <- split(strucs, strucs_values) # different from sibling
    s_attr_strucs <- s_attribute
  }
  if (verbose) cli_progress_done()

  if (is.character(values)){
    if (verbose) cli_progress_step("keep only matches for values")
    for (i in rev(which(!names(cpos_list) %in% values))){
      cpos_list[[i]] <- NULL
      struc_list[[i]] <- NULL
    }
    if (verbose) cli_progress_done()
  }

  .fn <- function(i){
    y <- prototype
    y@cpos <- matrix(cpos_list[[i]], ncol = 2L, byrow = FALSE)
    y@name <- names(cpos_list)[[i]]
    y@strucs <- struc_list[[i]]
    y@s_attribute_strucs <- s_attr_strucs
    if (relation == "sibling"){
      y@s_attributes <- c(
        x@s_attributes,
        setNames(list(names(cpos_list)[[i]]), s_attribute)
      )
    }
    y@xml = x@xml # to reconsider
    y@size = sum((y@cpos[,2] + 1L) - y@cpos[,1])
    y@type = x@type
    y
  }
  
  if (progress){
    if (verbose)
      cli_alert_info(
        "instantiate {.val {length(cpos_list)}} {new_class} objects"
      )
    y@objects <- pblapply(seq_along(cpos_list), .fn)
  } else {
    if (isFALSE(mc)){
      if (verbose) cli_progress_step(
        "instantiate {.val {length(cpos_list)}} {new_class} objects "
      )
      y@objects <- lapply(seq_along(cpos_list), .fn)
    } else {
      if (isTRUE(mc)) mc <- parallel::detectCores() - 1L
      if (verbose) cli_progress_step(
        "instantiate {.val {length(cpos_list)}} {new_class} objects using {.val {mc}} cores"
      )
      y@objects <- mclapply(seq_along(cpos_list), .fn, mc.cores = mc)
      
    }
    if (verbose) cli_progress_done()
  }

  if (nchar(prefix) == 0L){
    names(y@objects) <- names(cpos_list)
  } else {
    names(y) <- paste(prefix, names(cpos_list), sep = "_")
  }
  names(y@objects) <- sapply(y@objects, function(x) x@name)
  y
})


#' @examples
#' gparl <- corpus("GERMAPARLMINI")
#' b <- split(gparl, s_attribute = "date")
#' @exportMethod split
#' @rdname subcorpus_bundle
#' @inheritParams partition_bundle
setMethod("split", "corpus", function(
  x, s_attribute, values, prefix = "",
  mc = getOption("polmineR.mc"), verbose = TRUE, progress = FALSE,
  type = get_type(x), xml = "flat"
) {
  
  stopifnot(
    is.character(s_attribute),
    length(s_attribute) == 1L
  )
  if (!s_attribute %in% s_attributes(x))
    stop(sprintf("s-attribute '%s' not available", s_attribute))
  
  if (missing(values))
    # prospectively use RcppCWB::cl_struc_values()
    values <- s_attr_has_values(s_attribute = s_attribute, x = x)
  if (is.null(values)) values <- TRUE
  
  if (verbose) cli_alert_info(
    's-attribute {.val {s_attribute}} has values: {col_cyan({if (isFALSE(values)) "no" else "yes"})}'
  )
  
  
  # Ensure that when split() is called within partition_bundle(), the resulting 
  # object is a partition_bundle and the objects in the slot 'object' are 
  # partition objects, not subcorpus objects.
  history <- rev(sapply(sys.calls(), function(x) deparse(x[[1]])))
  pb_call <- if (5L %in% which(history == "partition_bundle")) TRUE else FALSE
  retval_class <- if (pb_call) "partition_bundle" else "subcorpus_bundle"
  cl <- if (pb_call) "partition" else "subcorpus"
  new_class <- if (is.na(x@type)) cl else paste(x@type, cl, sep = "_")
  
  y <- as(as(x, "corpus"), retval_class)

  struc_size <- cl_attribute_size(
    corpus = x@corpus, registry = x@registry_dir,
    attribute = s_attribute, attribute_type = "s"
  )
  strucs <- 0L:(struc_size - 1L)
  
  if (verbose) cli_progress_step("get regions and get values")
  cpos_matrix <- get_region_matrix(
    corpus = x@corpus, registry = x@registry_dir,
    s_attribute = s_attribute, strucs = strucs
  )
  
  if (isFALSE(values)){
    cpos_list <- split(cpos_matrix, strucs)
    struc_list <- split(strucs, strucs)
  } else {
    strucs_values <- struc2str(x = x, s_attr = s_attribute, struc = strucs)
    cpos_list <- split(cpos_matrix, strucs_values)
    struc_list <- split(strucs, strucs_values)
  }
  
  if (verbose) cli_progress_done()
  
  if (!is.null(values) && is.character(values)){
    if (verbose) cli_progress_step(
      "keep only matches for {col_cyan({length(values)})} values provided"
    )
    drop <- which(!names(cpos_list) %in% values)
    if (length(drop) > 0L){
      cpos_list <- cpos_list[-drop]
      struc_list <- struc_list[-drop]
    }
    if (verbose) cli_progress_done()
  }
  
  prototype <- as(x, new_class)
  
  .fn <- function(i){
    y <- prototype
    y@name <- names(cpos_list)[[i]]
    y@cpos <- matrix(cpos_list[[i]], ncol = 2L, byrow = FALSE)
    y@strucs <- struc_list[[i]]
    y@s_attributes <- setNames(list(names(cpos_list)[[i]]), s_attribute)
    y@s_attribute_strucs <- s_attribute
    y@xml <- xml
    y@size <- sum((y@cpos[,2] + 1L) - y@cpos[,1])
    y@type <- x@type
    y
  }
  
  if (verbose)
    cli_progress_step("instantiate objects (n = {.val {length(cpos_list)}})")
  
  y@objects <- if (progress)
    pblapply(seq_along(cpos_list), .fn, cl = mc)
  else
    lapply(seq_along(cpos_list), .fn)
  if (verbose) cli_progress_done()
  
  if (verbose) cli_progress_step("assign names")
  if (nchar(prefix) == 0L){
    names(y@objects) <- names(cpos_list)
  } else {
    names(y) <- paste(prefix, names(cpos_list), sep = "_")
  }
  names(y@objects) <- sapply(y@objects, function(x) x@name)
  if (verbose) cli_progress_done()
  
  y
})


#' @details Applying the `split`-method to a `subcorpus_bundle`-object
#'   will iterate through the subcorpus, and apply `split` on each
#'   `subcorpus` object in the bundle, splitting it up by the s-attribute
#'   provided by the argument `s_attribute`. The return value is a
#'   `subcorpus_bundle`, the names of which will be the names of the
#'   incoming `partition_bundle` concatenated with the s-attribute values
#'   used for splitting. The argument `prefix` can be used to achieve a
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


