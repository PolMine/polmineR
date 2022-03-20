#' @include S4classes.R
NULL


#' Get ranges for query.
#' 
#' Get ranges (pairs of left and right corpus positions) for queries.
#' 
#' @param mc If \code{logical} value \code{TRUE}, the value of
#'   \code{getOption("polmineR.cores")} is passed into \code{mclapply} or
#'   \code{pblapply} as the specification of the number of cores to use. It is
#'   also possible to supply an integer value with the number of cores directly.
#'   Defaults to 1 (no multicore). Relevant only if several queries are to be
#'   processed.
#' @param progress A \code{logical} value, whether to show a progess bar when
#'   processing multiple queries.
#' @inheritParams cpos
#' @rdname ranges
#' @exportMethod ranges
setGeneric("ranges", function(.Object, ...) standardGeneric("ranges"))

#' @rdname ranges
setMethod("ranges", "corpus", function(.Object, query, cqp = FALSE, check = TRUE, p_attribute = "word", verbose = TRUE, mc = 1L, progress = FALSE){
  if (is.logical(mc)) if (mc) mc <- getOption("polmineR.cores") else mc <- 1L
  .fn <- function(x){
    cpos(.Object = .Object, query = x, cqp = cqp, check = check, p_attribute = p_attribute, verbose = FALSE)
  }
  cpos_list <- if (progress) pblapply(as.list(query), .fn, cl = mc) else mclapply(as.list(query), .fn, mc.cores = mc)
  if (is.null(names(query))) names(cpos_list) <- query else names(cpos_list) <- names(query)
  for (i in length(query):1L) if (is.null(cpos_list[[i]])) cpos_list[[i]] <- NULL
  new(
    "ranges",
    corpus = .Object@corpus,
    encoding = .Object@encoding,
    cpos = do.call(rbind, cpos_list),
    query = unlist(lapply(names(cpos_list), function(x) rep(x, times = nrow(cpos_list[[x]]))))
  )
})


#' @rdname ranges
setMethod("ranges", "character", function(.Object, query, cqp = FALSE, check = TRUE, p_attribute = "word", verbose = TRUE, mc = 1L, progress = FALSE){
  ranges(.Object = corpus(.Object), query = query, cqp = cqp, check = check, p_attribute = p_attribute, verbose = verbose, mc = mc, progress = progress)
})


#' @rdname ranges
setMethod("ranges", "subcorpus", function(.Object, query, cqp = FALSE, check = TRUE, p_attribute = "word", verbose = TRUE, mc = 1L, progress = FALSE){
  x <- new(
    "corpus",
    corpus = .Object@corpus,
    data_dir = fs::path(.Object@data_dir),
    type = .Object@type,
    encoding = .Object@encoding,
    name = character(),
    size = size(.Object@corpus)
  )
  
  rng <- ranges(.Object = x, query = query, cqp = cqp, check = check, p_attribute = p_attribute, verbose = verbose, mc = mc, progress = progress)
  strucs_matches <- cl_cpos2struc(
    corpus = .Object@corpus,
    s_attribute = .Object@s_attribute_strucs,
    cpos = rng@cpos[,1], registry = registry()
  )
  rng@cpos <- rng@cpos[strucs_matches %in% .Object@strucs,]
  rng@query <- rng@query[strucs_matches %in% .Object@strucs]
  rng
})



#' @rdname ranges
setMethod("ranges", "partition", function(.Object, query, cqp = FALSE, check = TRUE, p_attribute = "word", verbose = TRUE, mc = 1L, progress = FALSE){
  ranges(.Object = as(.Object, "subcorpus"), query = query, cqp = cqp, check = check, p_attribute = p_attribute, verbose = verbose, mc = mc, progress = progress)
})

#' @param x A \code{ranges} class object.
#' @param ... Additional arguments (unused).
#' @rdname ranges_class
#' @export
#' @method as.data.table ranges
as.data.table.ranges <- function(x, ...){
  y <- data.table::as.data.table(x@cpos)
  colnames(y) <- c("cpos_left", "cpos_right")
  y[, "query" := x@query]
  y
}
