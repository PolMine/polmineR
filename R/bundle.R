#' @include textstat.R S4classes.R
NULL



setGeneric("as.bundle", function(object,...) standardGeneric("as.bundle"))

#' @rdname bundle
setMethod("length", "bundle", function(x) length(x@objects))


#' @rdname bundle
setMethod("names", "bundle", function(x) names(x@objects))


#' @rdname bundle
#' @exportMethod names<-
setReplaceMethod(
  "names",
  signature = c(x = "bundle", value = "vector"),
  function(x, value) {
    if ( !is.vector(value) ){
      stop("value needs to be a character vector")
    }
    if (is.list(value)) value <- unlist(value)
    if ( length(value) != length(x@objects) ) {
      warning("length of value provided does not match number of partitions")
      stop()
    }
    # for (i in 1L:length(x@objects)) x@objects[[i]]@name <- value[i]
    x@objects <- lapply(
      1L:length(x@objects),
      function(i) {p <- x@objects[[i]];  p@name <- value[i]; p}
    )
    names(x@objects) <- value
    x
  }
)


#' @rdname bundle
setMethod("unique", "bundle", function(x){
  objectNames <- names(x)
  uniqueObjectNames <- unique(objectNames)
  uniquePos <- sapply(uniqueObjectNames, function(x) grep(x, objectNames)[1])
  objectsToDrop <- which(1:length(objectNames) %in% uniquePos == FALSE)
  objectsToDrop <- objectsToDrop[order(objectsToDrop, decreasing=TRUE)]
  for (pos in objectsToDrop) x@objects[pos] <- NULL
  x
})




#' @exportMethod +
#' @rdname bundle
#' @param e1 object 1
#' @param e2 object 2
#' @docType methods
setMethod("+", signature(e1 = "bundle", e2 = "bundle"), function(e1, e2){
  newObjectClass <- unique(c(is(e1)[1], is(e2)[1]))
  if (length(newObjectClass) > 1) stop("the two objects do not have the same length")
  new(
    newObjectClass,
    objects=c(e1@objects, e2@objects),
    corpus=unique(e1@corpus, e2@corpus),
    encoding=unique(e1@encoding, e2@encoding)
    )
})

#' @exportMethod +
#' @rdname bundle
setMethod("+", signature(e1 = "bundle", e2 = "textstat"), function(e1, e2){
  e1@objects[[length(e1@objects)+1]] <- e2
  names(e1@objects)[length(e1@objects)] <- e2@name
  e1
})



#' @exportMethod [[
#' @rdname bundle
setMethod('[[', 'bundle', function(x,i){
  if (length(i) == 1L){
    return(x@objects[[i]])
  } else {
    return( as.bundle(lapply(i, function(j) x[[j]])) )
  }
})  

#' @exportMethod [[<-
#' @rdname bundle
#' @examples
#' p <- partition("GERMAPARLMINI", date = "2009-11-11")
#' pb <- partition_bundle(p, s_attribute = "party")
#' names(pb)
#' pb[["NA"]] <- NULL
#' names(pb)
setMethod("[[<-", "bundle", function(x,i, value){
  x@objects[[i]] <- value
  x
  }
)

#' @param name The name of an object in the \code{bundle} object.
#' @exportMethod $
#' @rdname bundle
#' @examples
#' pb <- partition_bundle("GERMAPARLMINI", s_attribute = "party")
#' pb$SPD # access partition names "SPD" in partition_bundle pb
setMethod("$", "bundle", function(x,name) x@objects[[name]])


#' @exportMethod $<-
#' @rdname bundle
#' @examples
#' pb <- partition_bundle("GERMAPARLMINI", s_attribute = "party")
#' pb$"NA" <- NULL # quotation needed if name is "NA"
setMethod("$<-", "bundle", function(x,name, value){
  x@objects[[name]] <- value
  x}
)

#' @exportMethod sample
#' @rdname bundle
setMethod("sample", "bundle", function(x, size) x[[sample(1:length(x), size = size)]])


setAs(from = "list", to = "bundle", def = function(from){
  unique_class <- unique(unlist(lapply(from, class)))
  stopifnot(length(unique_class) == 1L)
  
  if (grepl("subcorpus", unique_class)){
    new_object_class <- "subcorpus_bundle"
  } else if (grepl("[pP]artition", unique_class)){
    new_object_class <- "partition_bundle"
  } else if (unique_class == "kwic"){
    new_object_class <- "kwic_bundle"
  } else {
    new_object_class <- "bundle"
  }
  
  new(
    new_object_class,
    objects = setNames(from, nm = unlist(unname(lapply(from, function(x) x@name)))),
    corpus = unique(unlist(lapply(from, function(x) x@corpus))),
    encoding = unique(unlist(lapply(from, function(x) x@encoding)))
  )
})


#' @rdname bundle
#' @exportMethod as.bundle
setMethod("as.bundle", "list", function(object, ...){
  as(object, "bundle")
})

#' @docType methods
#' @exportMethod as.partition_bundle
#' @rdname bundle
setMethod("as.bundle", "textstat", function(object){
  retval <- new(
    paste(is(object)[1], "_bundle", sep = ""),
    objects = list(object),
    corpus = object@corpus,
    encoding = object@encoding,
    explanation = c("derived from a partition object")
  )
  names(retval@objects)[1] <- object@name
  retval
})


#' @examples
#' use("polmineR")
#' Ps <- partition_bundle(
#'   "REUTERS", s_attribute = "id",
#'   values = s_attributes("REUTERS", "id")
#' )
#' Cs <- cooccurrences(Ps, query = "oil", cqp = FALSE, verbose = FALSE, progress = TRUE)
#' dt <- polmineR:::as.data.table.bundle(Cs, col = "ll")
#' m <- as.matrix(Cs, col = "ll")
#' @export as.data.table.bundle
#' @rdname bundle
as.data.table.bundle <- function(x, col){
  pAttr <- unique(unlist(lapply(x@objects, function(i) i@p_attribute)))
  if (length(pAttr) > 1) stop("no unambigious p-attribute!")
  dts <- lapply(
    x@objects,
    function(object){
      data.table(
        name = object@name,
        token = object@stat[[object@p_attribute]],
        value = object@stat[[col]]
      )
    }
  )
  DT <- rbindlist(dts)
  y <- dcast(DT, token ~ name, value.var = "value")
  y
}


#' @rdname bundle
#' @exportMethod as.matrix
setMethod("as.matrix", "bundle", function(x, col){
  dt <- as.data.table(x = x, col = col)
  token <- dt[["token"]]
  dt[, "token" := NULL, with = TRUE]
  M <- as.matrix(dt)
  rownames(M) <- token
  M
})

#' @rdname bundle
setMethod("subset", "bundle", function(x, ...){
  for (i in 1L:length(x)) x@objects[[i]]@stat <- subset(x@objects[[i]]@stat, ...)
  x
})

#' @rdname bundle
#' @exportMethod as.list
setMethod("as.list", "bundle", function(x) x@objects)
