#' @examples
#' 
#' # subset a subcorpus_bundle
#' merkel <- corpus("GERMAPARLMINI") %>%
#'   split(s_attribute = "protocol_date") %>%
#'   subset(speaker == "Angela Dorothea Merkel")
#' 
#' # iterate over objects in bundle one by one 
#' sp <- corpus("GERMAPARLMINI") %>%
#'   as.speeches(
#'     s_attribute_name = "speaker",
#'     s_attribute_date = "protocol_date",
#'     progress = FALSE
#'   ) %>%
#'   subset(interjection == "speech", iterate = TRUE, progress = FALSE)
#' @rdname subset
#' @include partition_bundle.R
#' @param iterate A `logical` value, if `TRUE`, process very single object of
#'   `x` individually.
#' @param verbose A `logical` value, whether to show progress messages.
#' @param progress A `logical` value, whether to display progress bar.
#' @param mc An `integer` value, number of cores to use. If `NULL` (default),
#'   no multithreading.
#' @details The default approach for subsetting a `subcorpus_bundle` is to
#'   temporarily merge objects into a single `subcorpus`, perform `subset()`,
#'   and restore `subcorpus_bundle` by splitting on the s-attribute of the input
#'   `subcorpus_bundle`. This approach may have unintended results, if `x` has
#'   been generated using complex criteria. This may be the case for instance,
#'   if `x` resulted from `as.speeches()`. In this scenario, set argument
#'   `iterate` to `TRUE` to iterate over objects in bundle one-by-one.
setMethod("subset", "subcorpus_bundle", function(x, ..., iterate = FALSE, verbose = TRUE, progress = FALSE, mc = NULL){
  if (iterate){
    if (progress){
      x@objects <- pblapply(x@objects, function(obj) subset(obj, ...), cl = mc)
    } else {
      x@objects <- if (!is.null(mc))
        mclapply(x@objects, function(obj) subset(obj, ...), mc.cores = mc)
      else
        lapply(x@objects, function(obj) subset(obj, ...))
    }
    return(x)
  } else {
    merged <- merge(x)
    sub <- subset(merged, ...)
    
    if (nrow(sub@cpos) == 0L){
      cli_alert_warning("no objects left after subsetting, returning `NULL`")
      return(NULL)
    }
    
    s_attr <- unique(unlist(lapply(x, slot, "s_attribute_strucs")))
    if (length(s_attr) > 1L) stop("slot `s_attribute_strucs` not unique")
    if (verbose) cli_alert_info("split by s-attribute {.val {s_attr}}")
    y <- split(sub, s_attribute = s_attr)
    
    diff_n <- length(x) - length(y)
    if (diff_n != 0L)
      cli_alert_info(
        paste0(
          "objects in bundle after subsetting is {.val {length(y)}} ",
          "({.val {diff_n}} less than before) ")
        )
    
    return(y)
  }
})
