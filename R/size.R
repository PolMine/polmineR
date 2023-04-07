#' @include partition.R TermDocumentMatrix.R S4classes.R
NULL

#' Get Number of Tokens.
#' 
#' The method will get the number of tokens in a `corpus`, `partition` or
#' `subcorpus`, split up by an s-attribute if provided.
#' 
#' One or more s-attributes can be provided to get the dispersion of tokens
#' across one or more dimensions. If more than one `s_attribute` is provided and
#' the structure of s-attributes is nested, ordering attributes according to the
#' ascending tree structure is advised for performance reasons.
#' 
#' @param x An object to get size(s) for.
#' @param s_attribute A `character` vector with s-attributes (one or more).
#' @param verbose A `logical` value, whether to output messages.
#' @param ... Further arguments (used only for backwards compatibility).
#' @rdname size-method
#' @return If `.Object` is a corpus (a `corpus` object or specified by corpus
#'   id), an `integer` vector if argument `s_attribute` is `NULL`, a two-column
#'   `data.table` otherwise (first column is the s-attribute, second column:
#'   "size"). If `.Object` is a `subcorpus_bundle` or a `partition_bundle`, a
#'   `data.table` (with columns "name" and "size").
#' @seealso See \code{\link{dispersion}}-method for counts of hits. The
#'   \code{\link{hits}} method calls the `size`-method to get sizes of
#'   subcorpora.
#' @aliases size,slice-method
#' @examples
#' use("polmineR")
#' use(pkg = "RcppCWB", corpus = "REUTERS")
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

#' @importFrom RcppCWB s_attr_relationship
#' @rdname size-method
setMethod("size", "corpus", function(x, s_attribute = NULL, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "size(sAttribute)",
      with = "size(s_attribute)"
    )
    s_attribute <- list(...)[["sAttribute"]]
  }
  
  if (is.null(s_attribute)){
    corpus_size <- cl_attribute_size(
      corpus = x@corpus, registry = x@registry_dir,
      attribute = "word", attribute_type = "p"
    )
    return(corpus_size)
  } else {
    stopifnot(all(s_attribute %in% s_attributes(x)))
    
    all_siblings <- siblings(
      corpus = x@corpus,
      registry = x@registry_dir,
      s_attribute
    )
    if (is.na(all_siblings)) all_siblings <- TRUE
    
    if (all_siblings){
      
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
    } else {
      
      if (verbose)
        cli_progress_step("s-attributes are not siblings, get relationship")
      
      relationship <- sapply(
        1L:(length(s_attribute) - 1L),
        function(i){
          s_attr_relationship(
            s_attribute[[i]], s_attribute[[i + 1]],
            corpus = x@corpus, registry = x@registry_dir
          )
        }
      )
      if (verbose)
        cli_progress_done()
      
      if (all(relationship <= 0L) || all(relationship >= 0L)){
        if (all(relationship <= 0L) && verbose){
          cli_alert_info("order of s-attributes starts with deep descendents")
        } 
        
        if (all(relationship >= 0L) && verbose){
          cli_alert_info(
            "proceed with reversed order of s-attributes (descendents first)"
          )
          s_attribute <- rev(s_attribute)
        } 

        if (verbose)
          cli_progress_step(
            "get regions for s-attribute {.val {s_attribute[[1]]}}"
          )
        
        struc_size <- cl_attribute_size(
          corpus = x@corpus, registry = x@registry_dir,
          attribute_type = "s", attribute = s_attribute[[1]]
        )
        strucs_seq <- 0L:(struc_size - 1L)
        m <- RcppCWB::get_region_matrix(
          corpus = x@corpus, registry = x@registry_dir,
          s_attribute = s_attribute[[1]], strucs = strucs_seq
        )
        dt <- data.table(size = m[,2L] - m[,1L] + 1L)
        str <- struc2str(x = x, s_attr = s_attribute[[1]], struc = strucs_seq)
        dt[, (s_attribute[[1]]) := str]
        for (i in 2L:length(s_attribute)){
          if (verbose)
            cli_progress_step(
              "get values for s-attribute {.val {s_attribute[[i]]}}"
            )
          strucs <- cpos2struc(x, s_attr = s_attribute[[i]], cpos = m[,1])
          str <- struc2str(x, s_attr = s_attribute[[i]], struc = strucs)
          dt[, (s_attribute[[i]]) := str]
        }
        if (verbose) cli_progress_step("aggregate region sizes")
        y <- dt[, sum(size), by = eval(s_attribute), with = TRUE]
        setnames(x = y, old = "V1", new = "size")
        if (verbose) cli_progress_done()
      } else {
        
        if (verbose)
          cli_alert_info(
            "s-attributes not ordered according to tree structure (inefficient)"
          )
        
        dt <- data.table(cpos = 0L:(size(x) - 1L))
        
        for (s_attr in s_attribute){
          if (verbose) cli_progress_step("decode s-attribute {.val {s_attr}}")
          n_strucs <- cl_attribute_size(
            corpus = x@corpus,
            registry = x@registry_dir,
            attribute = s_attr,
            attribute_type = "s"
          )
          struc_vec <- 0L:(n_strucs - 1L)
          m <- get_region_matrix(
            corpus = x@corpus,
            registry = x@registry_dir,
            s_attribute = s_attr,
            strucs = struc_vec
          )
          ext <- data.table(cpos = RcppCWB::ranges_to_cpos(m))
          struc_values <- cl_struc2str(
            corpus = x@corpus,
            registry = x@registry_dir,
            s_attribute = s_attr,
            struc = struc_vec
          ) |>
            as.nativeEnc(from = x@encoding)
          unfolded <- rep(struc_values, times = m[,2] - m[, 1] + 1)
          ext[, (s_attr) := unfolded]
          
          if (verbose) cli_progress_step(
            "merge corpus position results for s-attribute {.val {s_attr}}"
          )
          
          dt[, (s_attr) := ext[dt, on = "cpos"][[s_attr]]]
          
        }
        dt[, "size" := 1L]
      }
      if (verbose) cli_progress_step("aggregate results and get sizes")
      y <- dt[, sum(size), by = eval(s_attribute), with = TRUE]
      setnames(x = y, old = "V1", new = "size")
      if (verbose) cli_progress_done()
      return(y)
    }
    return(y)
  }
})

#' @exportMethod size
#' @rdname size-method
setMethod("size", "character", function(x, s_attribute = NULL, verbose = TRUE, ...){
  size(corpus(x), s_attribute = s_attribute, verbose = verbose, ...)
})


#' @noRd
setMethod("size", "slice", function(x, s_attribute = NULL, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "size(sAttribute)",
      with = "size(s_attribute)"
    )
    s_attribute <- list(...)[["sAttribute"]]
  }
  
  if (is.null(s_attribute)){
    return( sum(as.integer(x@cpos[,2L]) - as.integer(x@cpos[,1L]) + 1L) )
  } else {
    stopifnot(all(s_attribute %in% s_attributes(x)))
    
    all_siblings <- siblings(
      corpus = x@corpus, registry = x@registry_dir,
      c(x@s_attribute_strucs, s_attribute)
    )
    if (all_siblings){
      .fn <- function(s_attr){
        str <- cl_struc2str(
          corpus = x@corpus, registry = x@registry_dir,
          s_attribute = s_attr, struc = x@strucs
        )
        as.nativeEnc(str, from = x@encoding) 
      }
      tab <- as.data.table(lapply(setNames(s_attribute, s_attribute), .fn))
      tab[, size := x@cpos[,2] - x@cpos[,1] + 1L]
      y <- tab[, sum(size), by = eval(s_attribute), with = TRUE]
    } else {
      
      struclist <- lapply(
        s_attribute, 
        function(s) cpos2struc(x = x, s_attr = s, cpos = x@cpos[,1])
      )
      
      regionslist <- lapply(
        seq_along(s_attribute),
        function(i){
          get_region_matrix(
            corpus = x@corpus, registry = x@registry_dir,
            s_attribute = s_attribute[i], strucs = struclist[[i]]
          )
        }
      )
      
      is_parent <- sapply(
        regionslist,
        function(m) all(m[,1] <= x@cpos[,1]) && all(m[,2] >= x@cpos[,2])
      )
      
      if (all(is_parent)){
        tab <- data.table(size = x@cpos[,2] - x@cpos[,1] + 1L)
        for (i in seq_along(struclist)){
          str <- cl_struc2str(
            corpus = x@corpus, registry = x@registry_dir,
            s_attribute = s_attribute[[i]], struc = struclist[[i]]
          )
          tab[, (s_attribute[[i]]) := as.nativeEnc(str, from = x@encoding)]
        }
      } else {
        if (verbose) message("... decoding nested s-attributes at token-level (potentially slow)")
        cpos <- ranges_to_cpos(x@cpos)
        if (length(s_attribute) == 1L){
          strucs <- cpos2struc(x = x, s_attr = s_attribute, cpos = cpos)
          tab <- data.table(struc = strucs)[, .N, by = "struc"]
          value <- struc2str(x = x, s_attr = s_attribute, struc = tab[["struc"]])
          tab[, (s_attribute) := value][, "struc" := NULL]
          setnames(tab, old = "N", new = "size")
          setcolorder(tab, neworder = s_attribute)
        } else {
          value_list <- lapply(
            s_attribute,
            function(s_attr){
              if (verbose) message("... decoding s-attribute: ", s_attr)
              strucs <- cpos2struc(x = x, s_attr = s_attr, cpos = cpos)
              struc2str(x = x, s_attr = s_attr, struc = strucs)
            }
          )
          names(value_list) <- s_attribute
          tab <- as.data.table(value_list)[, "size" := 1L]
        }
      }

      y <- tab[, sum(size), by = eval(s_attribute), with = TRUE]
    }
    setnames(y, old = "V1", new = "size")
    setkeyv(y, cols = s_attribute)
    y
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


#' @describeIn subcorpus Get the size of a `subcorpus` object from the
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

#' @details The `size()`-method for `features` objects will return a named list
#'   with the size of the corpus of interest ("coi"), i.e. the number of tokens
#'   in the window, and the reference corpus ("ref"), i.e. the number of tokens
#'   that are not matched by the query and that are outside the window.
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


