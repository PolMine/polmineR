#' @include partition.R S4classes.R
NULL

#' @rdname regions_class
#' @param s_attribute An s-attribute denoted by a length-one `character` vector
#'   for which regions shall be derived.
#' @exportMethod regions
setGeneric("regions", function(x, s_attribute) standardGeneric("regions"))

#' @rdname regions_class
#' @exportMethod regions
setMethod("regions", "corpus", function(x, s_attribute){
  
  y <- as(x, "regions")
  
  struc_size <- cl_attribute_size(
    corpus = x@corpus,
    attribute = s_attribute,
    attribute_type = "s",
    registry = x@registry_dir
  )

  y@cpos = get_region_matrix(
    corpus = x@corpus,
    s_attribute = s_attribute,
    strucs = 0L:(struc_size - 1L),
    registry = x@registry_dir
  )
  
  y
})

#' @rdname regions_class
setMethod("regions", "subcorpus", function(x, s_attribute){
  
  y <- as(x, "regions")
  
  is_sibling <- s_attr_is_sibling(
    x = s_attribute, y = x@s_attribute_strucs,
    corpus = x@corpus, registry = x@registry_dir
  )
  if (is_sibling) return(y)

  is_descendent <- s_attr_is_descendent(
    x = s_attribute, y = x@s_attribute_strucs,
    corpus = x@corpus, registry = x@registry_dir
  )
  if (!is_descendent) stop("s-attribute required to be a descendent of x")
  
  regions <- s_attr_regions(
    corpus = x@corpus, registry = x@registry_dir, data_dir = x@data_dir,
    s_attr = s_attribute
  )
  
  strucs <- cpos2struc(x = x, s_attr = x@s_attribute_strucs, cpos = regions[,1])
  y@cpos <- regions[which(strucs %in% x@strucs),]
  
  y
})



#' @details The `as.regions`-method coerces objects to a `regions`-object.
#' @param ... Further arguments.
#' @rdname regions_class
#' @exportMethod as.regions
setGeneric("as.regions", function(x, ...) standardGeneric("as.regions"))


#' @examples 
#' p <- partition("GERMAPARLMINI", speaker = "Angela Dorothea Merkel")
#' r <- as(p, "regions")
#' 
#' #' sc <- subset("GERMAPARLMINI", speaker == "Angela Dorothea Merkel")
#' r <- as(sc, "regions")
#' @noRd
setAs(from = "partition", to = "regions", function(from, to){
  y <- new("regions")
  slots_to_get <- slotNames(y)
  for (s in slots_to_get) slot(y, name = s) <- slot(from, name = s)
  type <- get_type(y@corpus)
  y@type <- if (length(type) > 0L) type else character()
  y
})



setAs(from = "regions", to = "partition", function(from, to){
  new(
    "partition",
    cpos = from@cpos,
    encoding = from@encoding,
    corpus = from@corpus,
    registry_dir = from@registry_dir, 
    data_dir = from@data_dir,
    info_file = from@info_file,
    template = from@template,
    stat = data.table()
  )
})

setAs(from = "subcorpus", to = "partition", function(from, to){
  new(
    "partition",
    cpos = from@cpos,
    encoding = from@encoding,
    corpus = from@corpus,
    registry_dir = from@registry_dir,
    data_dir = from@data_dir,
    info_file = from@info_file,
    template = from@template,
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
  
  .cpos_left_right <- function(.SD)
    list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
  
  DT_list <- list(
    left = subset(DT, DT[["position"]] < 0),
    right = subset(DT, DT[["position"]] > 0)
  )
  if (node) DT_list[["node"]] <- subset(DT, DT[["position"]] == 0)
  DT_regions <- rbindlist(
    lapply(DT_list, function(x) x[, .cpos_left_right(.SD), by = "match_id"])
  )
  
  setorderv(DT_regions, cols = "match_id")
  
  new(
    Class = "regions",
    cpos = as.matrix(DT_regions[, "match_id" := NULL]),
    corpus = x@corpus,
    registry_dir = x@registry_dir,
    data_dir = x@data_dir,
    info_file = x@info_file,
    template = x@template,
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
#' use(pkg = "RcppCWB", corpus = "REUTERS")
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
