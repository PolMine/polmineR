#' @include S4classes.R
NULL


#' Get ranges for query matches.
#' 
#' Get ranges (left and right corpus positions) of query matches.
#' 
#' @param mc If `TRUE`, the value of `getOption("polmineR.cores")` is passed
#'   into `mclapply()` or `pblapply()` as the specification of the number of
#'   cores to use. It is also possible to supply an integer value with the
#'   number of cores directly. Defaults to 1 (no multicore). Relevant only if
#'   several queries are to be processed.
#' @param progress A `logical` value, whether to show a progess bar when
#'   processing multiple queries.
#' @inheritParams cpos
#' @rdname ranges
#' @exportMethod ranges
#' @examples
#' use("RcppCWB")
#' 
#' oil <- corpus("REUTERS") %>% 
#'   ranges(query = '"crude" "oil"', cqp = TRUE) %>% 
#'   as.data.table()
#'   
#' # use ranges() to check which matches for two CQP queries occur within
#' # defined maximum distance
#' 
#' prices <- corpus("REUTERS") %>% 
#'   ranges(query = '"price.*"', cqp = TRUE) %>% 
#'   as.data.table()
#' 
#' is_close <- oil[, .(
#'   close = any(
#'     abs(cpos_left - prices$cpos_right) <= 10 |
#'     abs(cpos_right - prices$cpos_left) <= 10
#'   )),
#'   by = "cpos_left"
#' ]
#' oil_min <- oil[cpos_left %in% is_close[close == TRUE]$cpos_left]
setGeneric("ranges", function(.Object, ...) standardGeneric("ranges"))

#' @importFrom stringi stri_c_list
#' @rdname ranges
setMethod(
  "ranges", "corpus",
  function(
    .Object, query, cqp = FALSE, check = TRUE, p_attribute = "word",
    verbose = TRUE, mc = 1L, progress = FALSE
  ){
  if (is.logical(mc)) if (mc) mc <- getOption("polmineR.cores") else mc <- 1L
  .fn <- function(x) cpos(
    .Object = .Object,
    query = x,
    cqp = cqp,
    check = check,
    p_attribute = p_attribute,
    verbose = FALSE
  )

  if (!is.null(names(query))) if (any(nchar(names(query)) == 0L)) stop(
    "If vector query is named, all individual queries need to be named."
  )
  
  if (progress){
    cpos_list <- pblapply(as.list(query), .fn, cl = mc)
  } else {
    cpos_list <- mclapply(as.list(query), .fn, mc.cores = mc)
  }

  if (is.null(names(query))) names(cpos_list) <- query
  
  for (i in length(query):1L)
    if (is.null(cpos_list[[i]])) cpos_list[[i]] <- NULL
  
  y <- as(.Object, "ranges")
  region_matrix <- do.call(rbind, cpos_list)
  if (is.null(region_matrix)){ # no query matches at all
    slot(y, "cpos") <- matrix(nrow = 0L, ncol = 2L)
    slot(y, "query") <- character()
    slot(y, "match") <- character()
  } else {
    slot(y, "cpos") <- region_matrix
    slot(y, "query") <- unlist(
      lapply(names(cpos_list), function(x) rep(x, times = nrow(cpos_list[[x]])))
    )
    slot(y, "match") <- stri_c_list(get_token_stream(
      slot(y, "cpos"),
      p_attribute = p_attribute,
      corpus = slot(.Object, "corpus"),
      registry = slot(.Object, "registry_dir"),
      split = TRUE
    ), sep = " ")
  }

  slot(y, "size") <- integer() # drop corpus size
  slot(y, "size") <- size(y)
  y
})


#' @rdname ranges
setMethod(
  "ranges", "character",
  function(
    .Object, query, cqp = FALSE, check = TRUE, p_attribute = "word",
    verbose = TRUE, mc = 1L, progress = FALSE
  ){
  ranges(
    .Object = corpus(.Object),
    query = query,
    cqp = cqp,
    check = check,
    p_attribute = p_attribute,
    verbose = verbose,
    mc = mc,
    progress = progress
  )
})


#' @rdname ranges
setMethod("ranges", "subcorpus", function(.Object, query, cqp = FALSE, check = TRUE, p_attribute = "word", verbose = TRUE, mc = 1L, progress = FALSE){

  rng <- ranges(
    .Object = as(.Object, "corpus"),
    query = query, cqp = cqp, check = check,
    p_attribute = p_attribute,
    verbose = verbose, mc = mc, progress = progress
  )
  
  strucs_matches <- cl_cpos2struc(
    corpus = slot(.Object, "corpus"),
    s_attribute = slot(.Object, "s_attribute_strucs"),
    cpos = slot(rng, "cpos")[,1], registry = slot(.Object, "registry_dir")
  )
  slot(rng, "cpos") <- slot(rng, "cpos")[strucs_matches %in% slot(.Object, "strucs"),]
  slot(rng, "query") <- slot(rng, "query")[strucs_matches %in% slot(.Object, "strucs")]
  slot(rng, "match") <- if (nrow(slot(rng, "cpos")) == 0L){
    character()
  } else {
    stri_c_list(
      get_token_stream(
        slot(rng, "cpos"),
        p_attribute = p_attribute,
        corpus = slot(.Object, "corpus"),
        registry = slot(.Object, "registry_dir"),
        split = TRUE
      ),
      sep = " "
    )
  }

  slot(rng, "size") <- integer() # drop corpus size
  slot(rng, "size") <- size(rng)

  rng
})



#' @rdname ranges
setMethod(
  "ranges", "partition",
  function(
    .Object, query, cqp = FALSE, check = TRUE, p_attribute = "word",
    verbose = TRUE, mc = 1L, progress = FALSE
  ){
  ranges(
    .Object = as(.Object, "subcorpus"),
    query = query,
    cqp = cqp,
    check = check,
    p_attribute = p_attribute,
    verbose = verbose,
    mc = mc,
    progress = progress
  )
})

#' @param x A `ranges` class object.
#' @param ... Additional arguments (unused).
#' @rdname ranges_class
#' @export
#' @method as.data.table ranges
as.data.table.ranges <- function(x, ...){
  y <- data.table::as.data.table(slot(x, "cpos"))
  colnames(y) <- c("cpos_left", "cpos_right")
  y[, "query" := slot(x, "query")]
  y[, "match" := slot(x, "match")]
  y
}
