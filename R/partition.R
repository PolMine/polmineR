#' @include polmineR.R textstat.R count.R S4classes.R
NULL


# this file includes the partition class, the constructor function "partition"
# for generating the partition class, and the helper functions used 
# by the constructur

#' @exportMethod show
#' @docType methods
#' @noRd
setMethod("show", "partition", function(object){
  message("** partition object **")
  message(sprintf("%-20s", "corpus:"), object@corpus)
  message(sprintf("%-20s", "name:"), object@name)
  if (length(object@s_attributes) == 0L) {
    message(sprintf("%-20s", "s-attributes:"), "no specification")
  } else {
    s <- unlist(lapply(
      names(object@s_attributes),
      function(x) paste(x, "=", paste(object@s_attributes[[x]], collapse = "/"))
    ))
    message(sprintf("%-20s", "s-attributes:"), s[1])
    if (length(s) >= 2L) for (i in 2L:length(s)) message(sprintf("%-20s", ""), s[i], appendLF = TRUE)
  } 
  message(sprintf("%-20s", "cpos:"), appendLF = FALSE)
  if (nrow(object@cpos) == 0L) message("not available") else message(nrow(object@cpos), " pairs of corpus positions")
  message(sprintf("%-20s", "size:"), appendLF = FALSE)
  if (is.null(object@size)) message("not available") else message(object@size, " tokens")
  message(sprintf("%-20s", "count:"), appendLF = FALSE)
  if (length(object@p_attribute) == 0L) message("not available") else message("available for ", object@p_attribute)
})




setAs("partition", "data.table", function(from) data.table(count(from)) )

setOldClass("json")

#' @importFrom jsonlite fromJSON
setAs(
  from = "json", to = "partition",
  def = function(from){
    slotsToMake <- getSlots("partition")
    slotsToMake <- slotsToMake[-which(names(slotsToMake) %in% c("stat", "metadata", "call"))]
    partitionList <- fromJSON(from)
    y <- new("partition")
    for (x in names(slotsToMake)){
      slot(y, x) <- as(partitionList[[x]], slotsToMake[x])
    }
    y
  }
)

#' @importFrom jsonlite toJSON
setAs(
  from = "partition", to = "json",
  def = function(from){
    slotsToGet <- slotNames("partition")[-which(slotNames("partition") %in% c("stat", "metadata", "call"))]
    toJSON(lapply(setNames(slotsToGet, slotsToGet), function(x) slot(from, x)))
  }
)


#' @details The \code{is.partition} function returns a \code{logical} value
#'   whether \code{x} is a \code{partition}, or not.
#' @rdname partition_class
#' @export is.partition
is.partition <- function(x) "partition" %in% is(x)


#' @details \code{partition_add_cpos} will generate the matrix with corpus
#'   positions (regions) based on the s-attributes as defined in the respective
#'   slot of the partition object. The result is found in the slot \code{cpos}
#'   of the \code{partition} object that is returned.
#' @param partition partition object with list of s-attributes
#' @param xml either "flat" or "nested"
#' @param regex logical
#' @noRd
.partition_add_cpos <- function(.Object, xml = "flat", regex = FALSE){
  stopifnot(xml %in% c("flat", "nested"))
  stopifnot(is.logical(regex))
  if (xml == "flat"){
    # The function works nicely - potentially, it can be optimized, but I have tried many things.
    # Interestingly, the for-loop is more effective than a vectorized version
    # an Rcpp-implementation of struc2str is not faster
    # potential for optimization: struc2str
    maxAttr <- cl_attribute_size(
      corpus = .Object@corpus,  registry = .Object@registry_dir,
      attribute = .Object@s_attribute_strucs, attribute_type = "s"
    )
    meta <- data.frame(struc = 0L:(maxAttr - 1L), select = rep(0L, times = maxAttr))
    if (length(.Object@s_attributes) > 0) {
      for (s_attr in names(.Object@s_attributes)){
        meta[,2] <- as.vector(
          cl_struc2str(
            corpus = .Object@corpus, registry = .Object@registry_dir,
            s_attribute = s_attr, struc = meta[,1]
          )
        )
        Encoding(meta[,2]) <- .Object@encoding
        if (regex == FALSE) {
          meta <- meta[which(meta[,2] %in% .Object@s_attributes[[s_attr]]),]
        } else {
          lines <- lapply(.Object@s_attributes[[s_attr]], function(x) grep(x, meta[,2]))
          meta <- meta[unique(unlist(lines)),]
        }
      }
      if (nrow(meta) == 0) {
        warning(paste("no strucs found for the values provided for s-attribute", s_attr))
      }
    }
    if (nrow(meta) != 0) {
      .Object@cpos <- RcppCWB::get_region_matrix(
        corpus = .Object@corpus, registry = .Object@registry_dir,
        s_attribute = .Object@s_attribute_strucs,
        strucs = meta[, 1]
      )
      .Object@strucs <- as.integer(meta[, 1])
    } else {
      warning("returning a NULL object")
      .Object <- NULL    
    }
  } else if (xml == "nested"){
    s_attr_names <- rev(names(.Object@s_attributes))
    strucs <- 0L:(
      cl_attribute_size(
        corpus = .Object@corpus, registry = .Object@registry_dir,
        attribute = s_attr_names[1], attribute_type = "s"
      ) - 1L
    )
    s_attr_values <- cl_struc2str(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = s_attr_names[1], struc = strucs
    )
    Encoding(s_attr_values) <- .Object@encoding
    if (regex == FALSE) {
      strucs <- strucs[ which(s_attr_values %in% .Object@s_attributes[[ s_attr_names[1] ]]) ]
    } else {
      matchList <- lapply(
        .Object@s_attributes[[ s_attr_names[1] ]],
        function(x) grep(x, s_attr_values)
      )
      strucs <- strucs[ unique(unlist(matchList)) ]
    }
    
    # turn strucs into cpos matrix using RcppCWB
    cpos <- RcppCWB::get_region_matrix(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = s_attr_names[1], strucs = strucs
    )
    
    if (length(s_attr_names) > 1L){
      for (i in 2L:length(s_attr_names)){
        strucs <- cl_cpos2struc(
          corpus = .Object@corpus, registry = .Object@registry_dir,
          s_attribute = s_attr_names[i], cpos = cpos[,1]
        )
        s_attr_values <- cl_struc2str(
          corpus = .Object@corpus,  registry = .Object@registry_dir,
          s_attribute = s_attr_names[i], struc = strucs
        )
        Encoding(s_attr_values) <- .Object@encoding
        if (regex) {
          hits <- unique(unlist(lapply(.Object@s_attributes[[ s_attr_names[i] ]], function(x) grep(x, s_attr_values))))
        } else {
          hits <- which(s_attr_values %in% .Object@s_attributes[[ s_attr_names[i] ]])
        }
        cpos <- if (length(hits) > 1L) cpos[hits,] else matrix(cpos[hits,], nrow = 1L, ncol = 2L)

        strucs <- strucs[hits]
      }
    }
    .Object@strucs <- strucs
    .Object@cpos <- cpos
  }
  .Object
}


######################################################


#' Initialize a partition.
#' 
#' Create a subcorpus and keep it in an object of the \code{partition} class. If
#' defined, counts are performed for the p-attribute defined by the parameter
#' \code{p_attribute}.
#' 
#' The function sets up a \code{partition} object based on s-attribute values.
#' The s-attributes defining the partition can be passed in as a list, e.g.
#' \code{list(interjection="speech", year = "2013")}, or directly (see
#' examples).
#' 
#' The s-attribute values defining the partition may use regular expressions. To
#' use regular expressions, set the parameter regex to \code{TRUE}. Regular
#' expressions are passed into \code{grep}, i.e. the regex syntax used in R
#' needs to be used (double backlashes etc.). If regex is \code{FALSE}, the
#' length of the character vectors can be > 1, matching s-attributes are
#' identifies with the operator '%in%'.
#'
#' The XML imported into the CWB may be "flat" or "nested". This needs to be
#' indicated with the parameter \code{xml} (default is "flat"). If you generate
#' a \code{partition} based on a flat XML structure, some performance gain may be
#' achieved when ordering the s-attributes with decreasingly restrictive
#' conditions. If you have a nested XML, it is mandatory that the order of the
#' s-attributes provided reflects the hierarchy of the XML: The top-level
#' elements need to be positioned at the beginning of the list with the
#' s-attributes, the the most restrictive elements at the end.
#' 
#' If \code{p_attribute} is not NULL, a count of tokens in the corpus will be
#' performed and kept in the \code{stat}-slot of the partition-object. The
#' length of the \code{p_attribute} character vector may be 1 or more. If two or
#' more p-attributes are provided, The occurrence of combinations will be
#' counted. A typical scenario is to combine the p-attributes "word" or "lemma"
#' and "pos".
#' 
#' @param .Object A length-one character-vector, the CWB corpus to be used.
#' @param def A named list of character vectors of s-attribute values, the names
#'   are the s-attributes (see details and examples)
#' @param name A name for the new \code{partition} object, defaults to "".
#' @param encoding The encoding of the corpus (typically "LATIN1 or "(UTF-8)),
#'   if NULL, the encoding provided in the registry file of the corpus
#'   (charset="...") will be used.
#' @param p_attribute The p-attribute(s) for which a count is performed.
#' @param regex A logical value (defaults to FALSE).
#' @param xml Either 'flat' (default) or 'nested'.
#' @param decode Logical, whether to turn token ids to strings (set FALSE to
#'   minimize object size / memory consumption) in data.table with counts.
#' @param type A length-one character vector specifying the type of corpus / partition (e.g. "plpr")
#' @param mc Whether to use multicore (for counting terms).
#' @param verbose Logical, whether to be verbose.
#' @param ... Arguments to define partition (see examples). If \code{.Object} is
#'   a \code{remote_corpus} or \code{remote_partition} object, the three dots
#'   (\code{...}) are used to pass arguments. Hence, it is necessary to state
#'   the names of all arguments to be passed explicity.
#' @return An object of the S4 class \code{partition}.
#' @author Andreas Blaette
#' @seealso To learn about the methods available for objects of the class
#'   \code{partition}, see \code{\link{partition_class}},
#' @examples
#' use("polmineR")
#' spd <- partition("GERMAPARLMINI", party = "SPD", interjection = "speech")
#' kauder <- partition("GERMAPARLMINI", speaker = "Volker Kauder", p_attribute = "word")
#' merkel <- partition("GERMAPARLMINI", speaker = ".*Merkel", p_attribute = "word", regex = TRUE)
#' s_attributes(merkel, "date")
#' s_attributes(merkel, "speaker")
#' merkel <- partition(
#'   "GERMAPARLMINI", speaker = "Angela Dorothea Merkel",
#'   date = "2009-11-10", interjection = "speech", p_attribute = "word"
#'   )
#' merkel <- subset(merkel, !word %in% punctuation)
#' merkel <- subset(merkel, !word %in% tm::stopwords("de"))
#'    
#' # a certain defined time segment
#' days <- seq(
#'   from = as.Date("2009-10-28"),
#'   to = as.Date("2009-11-11"),
#'   by = "1 day"
#' )
#' period <- partition("GERMAPARLMINI", date = days)
#' @import methods
#' @exportMethod partition
#' @rdname partition
#' @aliases partition
setGeneric("partition", function(.Object, ...) standardGeneric("partition") )



#' @rdname partition
setMethod("partition", "corpus", function(
    .Object, def = NULL, name = "",
    encoding = NULL, p_attribute = NULL, regex = FALSE, xml = slot(.Object, "xml"),
    decode = TRUE, type = get_type(.Object), mc = FALSE, verbose = TRUE, ...
) {
  
  stopifnot(xml %in% c("nested", "flat"))
  
  dot_list <- list(...)
  if ("pAttribute" %in% names(dot_list)){
    p_attribute <- dot_list[["pAttribute"]]
    dot_list[["pAttribute"]] <- NULL
  }
  
  if (is.null(def)){
    if (length(dot_list) > 0L)
      def <- dot_list
    else 
      stop("No s_attributes defining the partition offered")
  }
  
  if (!all(names(def) %in% s_attributes(.Object))) stop("not all s-attributes are available")
  p <- as(.Object, paste(c(type, "partition"), collapse = "_"))
  p@name <- name
  p@xml <- xml
  
  p@s_attributes <- lapply(def, function(x) as.corpusEnc(x, corpusEnc = p@encoding))
  
  .message('get cpos and strucs', verbose = verbose)
  if (is.null(def)){
    stop("no s-attributes provided to define partition")
  } else {
    p@s_attribute_strucs <- names(def)[length(def)]
    p <- .partition_add_cpos(p, xml, regex)  
  }
  if (!is.null(p)) {
    # get partition size
    p@size <- size(p)
    if (!is.null(p_attribute)) if (p_attribute[1] == FALSE) p_attribute <- NULL
    if (!is.null(p_attribute)) {
      p <- enrich(p, p_attribute = p_attribute, verbose = verbose, decode = decode, mc = mc)
    }
  } else {
    warning("... setting up the partition failed (returning NULL object)")
  }
  p
})





#' @details If `.Object` is a length-one character vector, a subcorpus/partition
#'   for the corpus defined be `.Object` is generated.
#' @rdname partition
setMethod("partition", "character", function(
  .Object, def = NULL, name = "", encoding = NULL, p_attribute = NULL,
  regex = FALSE,  decode = TRUE, type = get_type(.Object), mc = FALSE,
  verbose = TRUE, ...
){
  x <- corpus(.Object)
  partition(
    .Object = x,
    def = def, name = name, encoding = encoding, p_attribute = p_attribute,
    regex = regex, xml = x@xml, decode = decode, type = type, mc = mc,
    verbose = verbose, ...
  )
})

#' @param slots Object slots that will be reported columns of \code{data.frame}
#'   summarizing \code{partition} objects in environment.
#' @details If \code{.Object} is an environment (typically \code{.GlobalEnv}),
#'   the \code{partition} objects present in the environment are listed.
#' @rdname partition
setMethod("partition", "environment", function(.Object, slots = c("name", "corpus", "size", "p_attribute")){
  partitionObjects <- .get_objects(class = "partition", envir = .Object)
  if (length(slots) > 0){
    retval <- data.frame(
      c(
        list(object = partitionObjects),
        lapply(
          setNames(slots, slots),
          function(x) sapply(
            partitionObjects,
            function(y){
              value <- slot(get(y, envir = .Object), x)
              if (length(value) == 0) NA else value
            }
          )
        )
      ),
      stringsAsFactors = FALSE
    )
    return(retval)
  } else {
    return(partitionObjects)
  }
})

#' @details If \code{.Object} is a \code{partition} object, a subcorpus of the
#'   subcorpus is generated.
#' @rdname partition
setMethod("partition", "partition", function(.Object, def = NULL, name = "", regex = FALSE, p_attribute = NULL, decode = TRUE, xml = NULL, verbose = TRUE, mc = FALSE, ...){
  
  dot_list <- list(...)
  if ("pAttribute" %in% names(dot_list)){
    p_attribute <- dot_list[["pAttribute"]]
    dot_list[["pAttribute"]] <- NULL
  }
  
  if (is.null(def)){
    if (length(dot_list) > 0L)
      def <- dot_list
    else 
      stop("No s_attributes defining the partition offered")
  }
  
  if (!all(names(def) %in% s_attributes(.Object))) stop(sprintf("some or all s-attributes provided are not available: %s", paste(names(def), collapse = ", ")))
  if (length(def) > 1L) stop("only one s-attribute allowed")
  if (!is.null(xml)) stopifnot(xml %in% c("flat", "nested"))
  

  y <- as(.Object, class(.Object)[1])
  y@xml <- if (is.null(xml)) .Object@xml else xml
  y@stat <- data.table()
  
  .message('Setting up partition', name, verbose = verbose)
  def <- lapply(def, function(x) as.corpusEnc(x, corpusEnc = .Object@encoding))  
  y@s_attributes <- c(.Object@s_attributes, def)
  y@s_attribute_strucs <- names(def)[1]
  
  .message('getting cpos and strucs', verbose = verbose)
  
  if (y@xml == "flat") {
    s_attr_values <- cl_struc2str(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = names(def), struc = .Object@strucs
    )
    Encoding(s_attr_values) <- y@encoding
    hits <- if (regex) grep(def[[1]], s_attr_values) else which(s_attr_values %in% def[[1]])
    cpos_matrix_new <- .Object@cpos[hits,]
    y@cpos <- switch(
      class(cpos_matrix_new)[1],
      "matrix" = cpos_matrix_new,
      "integer"= matrix(cpos_matrix_new, ncol = 2, byrow = TRUE)
    )
    y@strucs <- .Object@strucs[hits]
  } else if (y@xml == "nested") {
    cpos_vec <- ranges_to_cpos(.Object@cpos)
    strucs_new <- cl_cpos2struc(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = names(def)[1], cpos = cpos_vec
    )
    strucs_new_unique <- unique(strucs_new)
    s_attr_values <- cl_struc2str(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = names(def), struc = strucs_new_unique
    )
    Encoding(s_attr_values) <- .Object@encoding
    matching <- if (regex) grep(def[[1]], s_attr_values) else which(s_attr_values %in% def[[1]])
    y@strucs <- strucs_new_unique[matching]
    y@cpos <- RcppCWB::get_region_matrix(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      s_attribute = names(def), strucs = y@strucs
    )
  }
  y@size <- size(y)
  if (length(p_attribute) > 0) {
    y@stat <- count(.Object = y, p_attribute = p_attribute, decode = decode, mc = mc)@stat
    y@p_attribute <- p_attribute
  }
  y
})




#' @param node A logical value, whether to include the node (i.e. query matches) in the region matrix
#' generated when creating a \code{partition} from a \code{context}-object.
#' @rdname partition
setMethod("partition", "context", function(.Object, node = TRUE){
  stopifnot(is.logical(node))
  r <- as.regions(.Object, node = node)
  y <- as(object = r, Class = "partition")
  
  y@name <- .Object@name
  y@p_attribute = .Object@p_attribute
  y@size <- size(y)

  # Second, generate a list with data.table objects with counts
  DT <- copy(.Object@cpos)
  if (!node) DT <- subset(DT, DT[["position"]] != 0)
  y@stat <- DT[, .N, by = c("match_id", paste(.Object@p_attribute, "id", sep = "_"))]
  
  setnames(y@stat, old = "N", new = "count")
  for (p_attr in .Object@p_attribute){
    y@stat[[p_attr]] <- cl_id2str(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      p_attribute = p_attr, id = y@stat[[paste(p_attr, "id", sep = "_")]]
    )
  }
  y
})


#' @rdname partition
setMethod("partition", "remote_corpus", function(.Object, ...){
  p <- ocpu_exec(fn = "partition", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "corpus"), ...)
  y <- as(p, "remote_partition")
  y@server <- .Object@server
  y@restricted <- .Object@restricted
  y
})


#' @rdname partition
setMethod("partition", "remote_partition", function(.Object, ...){
  p <- ocpu_exec(fn = "partition", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "partition"), ...)
  y <- as(p, "remote_partition")
  y@restricted <- .Object@restricted
  y@server <- .Object@server
  y
})

