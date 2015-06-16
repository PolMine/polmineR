#' get term frequencies
#' 
#' Method to obtain term frequencies for one or multiple terms or queries. The
#' query may be formulated using regular expression syntax (method="grep"),
#' or may use the CQP syntax.
#' 
#' @param object either a partition or a partitionCluster object
#' @param query a character vector (one or multiple terms to be looked up)
#' @param method either "in", "grep" or "cqp" (defaults to "in")
#' @param mc logical, whether to use multicore (defaults to FALSE)
#' @param pAttribute if NULL, the pAttributes available in the partition object will be reported
#' @param rel logical, defaults to FALSE 
#' @param ... further parameters
#' @exportMethod tf
#' @docType methods
#' @rdname tf-method
#' @name tf
#' @aliases tf-method
#' @seealso tf
#' @examples
#' # generate a partition for testing 
#' test <- partition("PLPRBTTXT")
#' tf(test, "Wir") # get frequencies for one token
#' tf(test, c("Wir", "lassen", "uns")) # get frequencies for multiple tokens
#' tf(test, c("Zuwander.*", "Integration.*"), method="grep") # get frequencies using "grep"-method
#' tf("PLPRTXT", c("machen", "Integration"), "word")
setGeneric("tf", function(object, ...){standardGeneric("tf")})

#' @rdname tf-method
setMethod("tf", "partition", function(object, query, pAttribute=NULL, method="in", mc=F, verbose=T){
  if (is.null(pAttribute)) {
    pAttr <- slot(get("session", ".GlobalEnv"), "pAttribute")
  } else {
    pAttr <- pAttribute
  }
  if (is.character(query) == TRUE){
    bag <- list(query=.adjustEncoding(query, object@encoding))
    if (method == "in"){ 
      bag[["abs"]] <- object@tf[[pAttr]][query,"tf"]
      bag[["rel"]] <- object@tf[[pAttr]][query,"tf"]/object@size
      tab <- data.frame(bag)
    } else if (method == "grep"){
      bag <- lapply(query, function(query) {
        foo <- list()
        rowNo <- grep(query, rownames(object@tf[[pAttr]]))
        foo[["abs"]] <- sum(object@tf[[pAttr]][rowNo,"tf"])
        foo[["rel"]] <- foo[["abs"]]/object@size
        data.frame(what=names(foo), query, freq=do.call(rbind, foo))
      })
      tab <- do.call(rbind, bag)
      tab <- xtabs(freq~query+what, data=tab)
      tab <- as.data.frame(as.matrix(unclass(tab)))
      colOrder <- unlist(
        lapply(names(object@tf),
               function(x) c(paste(x,"Abs", sep=""), paste(x, "Rel", sep="")))
      )
      tab <- tab[,colOrder]
    } else if (method == "cqp") {
      .getNumberOfHits <- function(query) {
          if (verbose == TRUE) message("... processing query ", query)
          cposResult <- cpos(.Object=object, query=query, pAttribute=pAttr, verbose=FALSE)
          if (is.null(cposResult)){
            retval <- 0
          } else {
            retval <- nrow(cposResult)
          }
          retval
        }
      if (mc == FALSE){
        no <- vapply(query, .getNumberOfHits, FUN.VALUE=1)
      } else if (mc == TRUE){
        no <- unlist(mclapply(
          query, .getNumberOfHits,
          mc.cores=slot(get("session", ".GlobalEnv"), "cores")
          ))
      }
      bag[["abs"]] <- no
      bag[["rel"]] <- no/object@size
      tab <- data.frame(bag)
      rownames(tab) <- NULL
    } else {
      warning("not a valid method specification")      
    }
  } else if (is.numeric(query)) {
    warning("tf method not implemented for token ids")
  }
  tab
})


#' @rdname tf-method
setMethod("tf", "partitionCluster", function(object, query, pAttribute=NULL, method="in", rel=FALSE){
  # check whether all partitions in the cluster have a proper label
  if (is.null(names(object@objects)) || any(is.na(names(object@objects)))) {
    warning("all partitions in the cluster need to have a label (at least some missing)")
  }
  what <- paste(pAttribute, ifelse(rel==FALSE, "Abs", "Rel"), sep="")
  tfAvailable <- unique(unlist(lapply(object@objects, function(x) names(x@tf))))
  if (method %in% c("in", "grep")){
    if (length(pAttribute)!=1) {
      warning("pAttribute required for methods 'in' and/or 'grep' but not given")
      if (pAttribute %in% tfAvailable == FALSE) {
        warning("requested term frequencies are not available")
      }
    }
  }
  bag <- lapply(
    names(object@objects),
    function(x) {
      data.frame(
        partition=x,
        query=query,
        tf(object@objects[[x]], query, method=method, pAttribute=pAttribute)
      )
    }
  )
  tab <- do.call(rbind, bag)
  if(!is.null(tab)){
    tab <- data.frame(tab[,c("partition", "query", what)])
    colnames(tab) <- c("partition", "query", "tf")
    tab <- xtabs(tf~partition+query, data=tab)
    tab <- as.data.frame(as.matrix(unclass(tab)))
  }
  tab
})

#' @rdname tf-method
setMethod("tf", "character", function(object, query, pAttribute=NULL, method="in", verbose=TRUE){
  if (is.null(pAttribute)) {
    pAttribute <- slot(get("session", '.GlobalEnv'), 'pAttribute')
    if (verbose == TRUE) message("... using pAttribute ", pAttribute, " from session settings")
  }  
  if (object %in% cqi_list_corpora()) {
    if (method=="in"){
      sAttr <- paste(object, ".", pAttribute, sep="")
      total <- cqi_attribute_size(sAttr)
      abs <- sapply(query, function(query) {
        cqi_id2freq(sAttr, cqi_str2id(sAttr, query))
      })
      rel <- abs/total
      tab <- data.frame(query=query, abs=abs, rel=rel, row.names=NULL)
    } else if (method=="cqp"){
      warning("not yet implemented")
    }
  } else {
    warning("the character string provided does not refer to an available corpus")
  }
  tab
})

#' @rdname context-class
setMethod("tf", "context", function(object) {
  object@frequency
})

setMethod("tf", "dispersion", function(object) object$abs)
