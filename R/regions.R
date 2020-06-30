#' @include partition.R S4classes.R
NULL

#' @details The \code{as.regions}-method coerces objects to a \code{regions}-object.
#' @param ... Further arguments.
#' @rdname regions_class
#' @exportMethod as.regions
setGeneric("as.regions", function(x, ...) standardGeneric("as.regions"))


#' @examples 
#' p <- partition("GERMAPARLMINI", speaker = "Angela Dorothea Merkel")
#' r <- as(p, "regions")
#' @noRd
setAs(from = "partition", to = "regions", function(from, to){
  y <- new("regions")
  slots_to_get <- slotNames(y)
  slots_to_get <- slots_to_get[-which(slots_to_get %in% c("data_dir", "type"))]
  for (s in slots_to_get) slot(y, name = s) <- slot(from, name = s)
  type <- get_type(y@corpus)
  y@type <- if (length(type) > 0L) type else character()
  y
})

#' @examples 
#' sc <- subset("GERMAPARLMINI", speaker == "Angela Dorothea Merkel")
#' r <- as(sc, "regions")
#' @noRd
setAs(from = "subcorpus", to = "regions", function(from, to){
  y <- new("regions")
  slots_to_get <- slotNames(y)
  slots_to_get <- slots_to_get[-which(slots_to_get == "type")]
  for (s in slots_to_get) slot(y, name = s) <- slot(from, name = s)
  y
})


setAs(from = "regions", to = "partition", function(from, to){
  new(
    "partition",
    cpos = from@cpos,
    encoding = from@encoding,
    corpus = from@corpus,
    stat = data.table()
  )
})

setAs(from = "subcorpus", to = "partition", function(from, to){
  new(
    "partition",
    cpos = from@cpos,
    encoding = from@encoding,
    corpus = from@corpus,
    strucs = from@strucs,
    s_attribute_strucs = from@s_attribute_strucs,
    xml = from@xml,
    size = size(from),
    stat = data.table(),
    ###
    name = character(),
    s_attributes = list(),
    explanation = character(),
    annotations = list(),
    metadata = data.frame(),
    p_attribute = character(),
    key = character(),
    call = character()
  )
})


#' @rdname partition_class
setMethod("as.regions", "partition", function(x) as(x, "regions"))

#' @param node A logical value, whether to include the node (i.e. query matches) in the region matrix
#' generated when creating a \code{partition} from a \code{context}-object.
#' @rdname context-class
setMethod("as.regions", "context", function(x, node = TRUE){
  DT <- copy(x@cpos)
  setkeyv(x = DT, cols = c("match_id", "cpos"))
  .cpos_left_right <- function(.SD) list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
  DT_list <- list(left = subset(DT, DT[["position"]] < 0), right = subset(DT, DT[["position"]] > 0))
  if (node) DT_list[["node"]] <- subset(DT, DT[["position"]] == 0)
  DT_regions <- rbindlist(lapply(DT_list, function(x) x[, .cpos_left_right(.SD), by = "match_id"]))
  setorderv(DT_regions, cols = "match_id")
  new(
    Class = "regions",
    cpos = as.matrix(DT_regions[, "match_id" := NULL]),
    corpus = x@corpus,
    encoding = x@encoding
  )
})



#' @details The \code{as.data.table} method returns the matrix with corpus
#'   positions in the slot \code{cpos} as a \code{data.table}.
#' @param keep.rownames Required argument to safeguard consistency with S3
#'   method definition in the \code{data.table} package. Unused in this context.
#' @rdname regions_class
#' @export
#' @method as.data.table regions
#' @examples
#' 
#' # Get regions matrix as data.table, without / with values
#' sc <- corpus("REUTERS") %>% subset(grep("saudi-arabia", places))
#' regions_dt <- as.data.table(sc)
#' regions_dt <- as.data.table(
#'   sc,
#'   values = s_attributes(sc, "id", unique = FALSE)
#' )
as.data.table.regions <- function(x, keep.rownames, values = NULL, ...){
  if (!missing(keep.rownames)){
    warning(
      "The argument 'keep.rownames' of the 'as.data.table' method for 'regions' class ",
      "objects or objects inheriting from the 'regions' class will not be used. It is ",
      "used in the method definition as a matter of consistency with the data.table package."
    )
  }
  if (length(list(...)) > 0L){
    warning(
      "Further arguments passed into the as.data.table method for regions class objects ",
      "or objects inheriting from the region class remain unused."
    )
  }
  dt <- data.table::as.data.table(x@cpos)
  if (!is.null(values)){
    stopifnot(length(values) == nrow(dt) || length(values) == 1)
    dt[[3]] <- values
  }
  dt
}