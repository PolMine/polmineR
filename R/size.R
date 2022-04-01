#' @include partition.R TermDocumentMatrix.R S4classes.R
NULL

#' Get Number of Tokens.
#' 
#' The method will get the number of tokens in a `corpus`, `partition` or
#' `subcorpus`, split up by an s-attribute if provided.
#' 
#' One or more s-attributes can be provided to get the dispersion of tokens
#' across one or more dimensions. If the corpus XML is nested, the s-attributes
#' defining a `subcorpus` or `partition` are required to be children of the
#' s-attribute(s) provided. If, not, a warning will be issued.
#' 
#' @param x An object to get size(s) for.
#' @param s_attribute A \code{character} vector with s-attributes (one or more).
#' @param verbose A \code{logical} value, whether to output messages.
#' @param ... Further arguments (used only for backwards compatibility).
#' @rdname size-method
#' @return If \code{.Object} is a corpus (a \code{corpus} object or specified by
#'   corpus id), an \code{integer} vector if argument \code{s_attribute} is
#'   \code{NULL}, a two-column \code{data.table} otherwise (first column is the
#'   s-attribute, second column: "size"). If \code{.Object} is a
#'   \code{subcorpus_bundle} or a \code{partition_bundle}, a \code{data.table}
#'   (with columns "name" and "size").
#' @seealso See \code{\link{dispersion}}-method for counts of hits. The \code{\link{hits}}
#' method calls the \code{size}-method to get sizes of subcorpora.
#' @aliases size,slice-method
#' @examples
#' use("polmineR")
#' 
#' # for corpus object
#' corpus("REUTERS") %>% size()
#' corpus("REUTERS") %>% size(s_attribute = "id")
#' corpus("GERMAPARLMINI") %>% size(s_attribute = c("date", "party"))
#' 
#' # for corpus specified by ID
#' size("GERMAPARLMINI")
#' size("GERMAPARLMINI", s_attribute = "date")
#' size("GERMAPARLMINI", s_attribute = c("date", "party"))
#' 
#' # for partition object
#' P <- partition("GERMAPARLMINI", date = "2009-11-11")
#' size(P, s_attribute = "speaker")
#' size(P, s_attribute = "party")
#' size(P, s_attribute = c("speaker", "party"))
#' 
#' # for subcorpus
#' sc <- corpus("GERMAPARLMINI") %>% subset(date == "2009-11-11")
#' size(sc, s_attribute = "speaker")
#' size(sc, s_attribute = "party")
#' size(sc, s_attribute = c("speaker", "party"))
#' 
#' # for subcorpus_bundle
#' subcorpora <- corpus("GERMAPARLMINI") %>% split(s_attribute = "date")
#' size(subcorpora)
setGeneric("size", function(x, ...) UseMethod("size"))

#' @rdname size-method
setMethod("size", "corpus", function(x, s_attribute = NULL, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (is.null(s_attribute)){
    return(
      cl_attribute_size(
        corpus = x@corpus, registry = x@registry_dir,
        attribute = "word", attribute_type = "p"
      )
    )
  } else {
    stopifnot(all(s_attribute %in% s_attributes(x)))
    dt <- data.table::as.data.table(
      lapply(
        setNames(s_attribute, s_attribute),
        function(s_attr){
          s_attr_max <- cl_attribute_size(
            corpus = x@corpus, registry = x@registry_dir,
            attribute = s_attr, attribute_type = "s"
          )
          s_attr_vals <- cl_struc2str(
            corpus = x@corpus, registry = x@registry_dir,
            s_attribute = s_attr, struc = 0L:(s_attr_max - 1L)
          )
          as.nativeEnc(s_attr_vals, from = x@encoding)
        }
      )
    )
    cpos_matrix <- RcppCWB::get_region_matrix(
      corpus = x@corpus, s_attribute = s_attribute[1],
      strucs = 0L:(
        cl_attribute_size(
          corpus = x@corpus, registry = x@registry_dir,
          attribute = s_attribute[1], attribute_type = "s"
          ) - 1L
      ),
      registry = x@registry_dir
    )
    
    dt[, size := cpos_matrix[,2] - cpos_matrix[,1] + 1L]
    y <- dt[, sum(size), by = eval(s_attribute), with = TRUE]
    setnames(y, old = "V1", new = "size")
    setkeyv(y, cols = s_attribute)
    return(y)
  }
})

#' @exportMethod size
#' @rdname size-method
setMethod("size", "character", function(x, s_attribute = NULL, verbose = TRUE, ...){
  size(corpus(x), s_attribute = s_attribute, verbose = verbose, ...)
})


#' @noRd
setMethod("size", "slice", function(x, s_attribute = NULL, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (is.null(s_attribute)){
    return( sum(as.integer(x@cpos[,2L]) - as.integer(x@cpos[,1L]) + 1L) )
  } else {
    stopifnot(all(s_attribute %in% s_attributes(x)))
    
    # Check whether s-attributes are nested by approximation: If s-attributes
    # have same number of values, we assume that they cover same regions.
    s_attr_sizes <- lapply(
      c(x@s_attribute_strucs, s_attribute),
      function(s_attr){
        cl_attribute_size(
          corpus = x@corpus, registry = x@registry_dir,
          attribute = s_attr, attribute_type = "s" 
        )
      }
    )
    if (do.call(identical, s_attr_sizes)){
      .fn <- function(s_attr){
        str <- cl_struc2str(
          corpus = x@corpus, registry = x@registry_dir,
          s_attribute = s_attr, struc = x@strucs
        )
        as.nativeEnc(str, from = x@encoding) 
      }
      dt <- data.table::as.data.table(lapply(setNames(s_attribute, s_attribute), .fn))
      dt[, size := x@cpos[,2] - x@cpos[,1] + 1L]
      y <- dt[, sum(size), by = eval(s_attribute), with = TRUE]
    } else {
      dt <- data.table(size = x@cpos[,2] - x@cpos[,1] + 1L)
      for (s_attr in s_attribute){
        strucs <- cl_cpos2struc(
          corpus = x@corpus, registry = x@registry_dir,
          s_attribute = s_attr, cpos = x@cpos[,1]
        )
        m <- get_region_matrix(
          corpus = x@corpus, registry = x@registry_dir,
          s_attribute = s_attr, strucs = strucs
        )
        if (all(m[,1] <= x@cpos[,1]) && all(m[,2] >= x@cpos[,2])){
          str <- cl_struc2str(
            corpus = x@corpus, registry = x@registry_dir,
            s_attribute = s_attr, struc = strucs
          )
          dt[, (s_attr) := as.nativeEnc(str, from = x@encoding)]
        } else {
          warning(
            sprintf(
              "Structural attribute '%s' is required to be a child of attribute '%s' - not true.",
              x@s_attribute_strucs, s_attr
            )
          )
        }
      }
      y <- dt[, sum(size), by = eval(s_attribute), with = TRUE]
    }
    setnames(y, old = "V1", new = "size")
    setkeyv(y, cols = s_attribute)
    return( y )
  }
})


#' @rdname size-method
setMethod("size", "partition", function(x, s_attribute = NULL, ...){
  callNextMethod(x = x, s_attribute = s_attribute, ...)
})

#' @rdname size-method
setMethod("size", "partition_bundle", function(x){
  cpos_list <- lapply(x@objects, slot, "cpos")
  cpos_matrix <- do.call(rbind, cpos_list)
  dt <- data.table(cpos_matrix)
  setnames(dt, old = c("V1", "V2"), new = c("cpos_left", "cpos_right"))
  dt[, "name" := unlist(Map(f = rep, x = lapply(x@objects, slot, "name"), times = lapply(cpos_list, nrow)))]
  dt[, "region_size" := dt[["cpos_right"]] - dt[["cpos_left"]] + 1L]
  y <- dt[, sum(.SD[["region_size"]]), by = "name"]
  setnames(y, old = "V1", new = "size")
  y
})


#' @describeIn subcorpus Get the size of a \code{subcorpus} object from the
#'   respective slot of the object.
setMethod("size", "subcorpus", function(x, s_attribute = NULL, ...){
  callNextMethod()
})


#' @rdname size-method
setMethod("size", "DocumentTermMatrix", function(x){
  setNames(tapply(x$v, INDEX = x$i, sum), x[["dimnames"]][["Docs"]])
})

#' @rdname size-method
setMethod("size", "TermDocumentMatrix", function(x){
  setNames(tapply(x$v, INDEX = x$j, sum), x[["dimnames"]][["Docs"]])
})

#' @details The \code{size}-method for \code{features} objects will return a
#'   named list with the size of the corpus of interest ("coi"), i.e. the number
#'   of tokens in the window, and the reference corpus ("ref"), i.e. the number
#'   of tokens that are not matched by the query and that are outside the
#'   window.
#' @rdname size-method
setMethod("size", "features", function(x) list(coi = x@size_coi, ref = x@size_ref) )


#' @rdname size-method
setMethod("size", "remote_corpus", function(x){
  ocpu_exec(fn = "size", corpus = x@corpus, server = x@server, restricted = x@restricted, do.call = FALSE, x = as(x, "corpus"))
})

#' @rdname size-method
setMethod("size", "remote_partition", function(x){
  ocpu_exec(fn = "size", corpus = x@corpus, server = x@server, restricted = x@restricted, do.call = FALSE, x = as(x, "partition"))
})


