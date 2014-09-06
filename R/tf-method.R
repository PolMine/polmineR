#' get term frequencies
#' 
#' Method to obtain term frequencies for one or multiple terms or queries.
#' The method can be applied to partition or partitionCluster class objects.
#' If object is a character string, frequencies for a whole corpus are returned.
#' Please see the respective documentation for details 
#' (\code{method?tf("partition")}, \code{method?tf("partitionCluster")} or
#' \code{method?tf("character")}).
#' 
#' @param object either a partition or a partitionCluster object
#' @param ... further parameters
#' @aliases tf tf-method
#' @rdname tf-method
setGeneric("tf", function(object, ...){standardGeneric("tf")})

#' get term frequencies from a partition object
#' 
#' Obtain term frequencies for one or several terms from a partition object.
#' 
#' The term frequencies are retrieved for all the p-attributes that are available 
#' in the partition object. To get the total frequency of the variations of a term,
#' use the "grep"-method.
#' 
#' @param object a partition object
#' @param token a character vector (one or multiple terms to be looked up)
#' @param method either "in", "grep" or "cqp" (defaults to "in")
#' @param pAttribute if NULL, the pAttributes available in the partition object will be reported
#' @return a data frame
#' @examples
#' # generate a partition for testing 
#' test <- partition("PLPRBTTXT", list(text_date=".*"), tf=c("word", "lemma"), method="grep")
#' tf(test, "Wir") # get frequencies for one token
#' tf(test, c("Wir", "lassen", "uns")) # get frequencies for multiple tokens
#' tf(test, c("Zuwander.*", "Integration.*"), method="grep") # get frequencies using "grep"-method
#' @rdname tf-partition-method
#' @aliases tf,partition-method
#' @exportMethod tf
#' @docType methods
setMethod("tf", "partition", function(object, token, method="in", pAttribute=NULL){
  if (is.null(pAttribute)){
    if (is.null(names(object@tf)) == TRUE) {
      warning("no pAttribute provided, no tf lists available")
    } else {
      pAttribute <- names(object@tf)
    }
  } else {
    if (method!="cqp" && any(!pAttribute %in% names(object@tf))){
      warning("pAttribute provided is not available")
    }
  }
  if (is.character(token) == TRUE){
    bag <- list(token=.adjustEncoding(token, object@encoding))
    if (method == "in"){ 
      for (pAttr in pAttribute) {
        bag[[paste(pAttr, "Abs", sep="")]] <- object@tf[[pAttr]][token,"tf"]
        bag[[paste(pAttr, "Rel", sep="")]] <- object@tf[[pAttr]][token,"tf"]/object@size
      }
      tab <- data.frame(bag)
    } else if (method == "grep"){
      bag <- lapply(token, function(query) {
        foo <- list()
        for (pAttr in pAttribute) {
          rowNo <- grep(query, rownames(object@tf[[pAttr]]))
          tfAbs <- sum(object@tf[[pAttr]][rowNo,"tf"])
          foo[[paste(pAttr, "Abs", sep="")]] <- tfAbs
          foo[[paste(pAttr, "Rel", sep="")]] <- tfAbs/object@size
        }
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
      for (pAttr in pAttribute) {
        no <- vapply(
          token,
          function(query) {
            n <- nrow(.queryCpos(query=query, Partition=object, pAttribute=pAttr, verbose=FALSE))
            ifelse(is.null(n), 0, n)
          }, FUN.VALUE=1
        )
        bag[[paste(pAttr, "Abs", sep="")]] <- no
        bag[[paste(pAttr, "Rel", sep="")]] <- no/object@size
      }
      tab <- data.frame(bag)
      rownames(tab) <- NULL
    } else {
      warning("not a valid method specification")      
    }
  } else if (is.numeric(token)) {
    warning("tf method not implemented for token ids")
  }
  tab
})


#' get term frequencies from a partitionCluster object
#' 
#' Obtain term frequencies from a \code{partitionCluster}. You get a data frame with the
#' partitions in the object in the rows and the different terms in the columns. It needs
#' to be specified whether relative or absolute frequencies are retrieved, and what
#' s-attribute will be queried.
#' 
#' @param object a partitionCluster object
#' @param token one or several tokens
#' @param method either "in", "grep" or "cqp"
#' @param pAttribute the p-attribute you want to get
#' @param rel logical, defaults to FALSE 
#' @return a data frame (partitions in the rows, terms in the columns)
#' @aliases tf,partitionCluster-method
#' @rdname tf-partitionCluster-method
#' @exportMethod tf
#' @docType methods
setMethod("tf", "partitionCluster", function(object, token, method="in", pAttribute=NULL, rel=FALSE){
  # check whether all partitions in the cluster have a proper label
  if (is.null(names(object@partitions)) || any(is.na(names(object@partitions)))) {
    warning("all partitions in the cluster need to have a label (at least some missing)")
  }
  what <- paste(pAttribute, ifelse(rel==FALSE, "Abs", "Rel"), sep="")
  tfAvailable <- unique(unlist(lapply(object@partitions, function(x) names(x@tf))))
  if (method %in% c("in", "grep")){
    if (length(pAttribute)!=1) {
      warning("pAttribute required for methods 'in' and/or 'grep' but not given")
      if (pAttribute %in% tfAvailable == FALSE) {
        warning("requested term frequencies are not available")
      }
    }
  }
  bag <- lapply(
    names(object@partitions),
    function(x) {
      data.frame(
        partition=x,
        token=token,
        tf(object@partitions[[x]], token, method=method, pAttribute=pAttribute)
      )
    }
  )
  tab <- do.call(rbind, bag)
  if(!is.null(tab)){
    tab <- data.frame(tab[,c("partition", "token", what)])
    colnames(tab) <- c("partition", "token", "tf")
    tab <- xtabs(tf~partition+token, data=tab)
    tab <- as.data.frame(as.matrix(unclass(tab)))
  }
  tab
})

#' get term frequencies from a corpus
#' 
#' Obtain term frequencies from a CWB corpus. You get a data frame with the
#' partitions in the object in the rows and the different terms in the columns.
#' 
#' @param object a character string specifying a CWB corpus
#' @param token one or several tokens
#' @param pAttribute the p-attribute you want to get
#' @param method either "in" or "cqp" (cqp method is not yet implemented)
#' @return a data frame (partitions in the rows, terms in the columns)
#' @examples
#' tf("PLPRTXT", c("machen", "Integration"), "word")
#' @aliases tf,character-method
#' @rdname tf-character-method
#' @exportMethod tf
#' @docType methods
setMethod("tf", "character", function(object, token, pAttribute, method="in"){
  if (object %in% cqi_list_corpora()) {
    if (method=="in"){
      sAttr <- paste(object, ".", pAttribute, sep="")
      total <- cqi_attribute_size(sAttr)
      abs <- sapply(token, function(query) {
        cqi_id2freq(sAttr, cqi_str2id(sAttr, query))
      })
      rel <- abs/total
      tab <- data.frame(token=token, abs=abs, rel=rel, row.names=NULL)
    } else if (method=="cqp"){
      warning("not yet implemented")
    }
  } else {
    warning("the character string provided does not refer to an available corpus")
  }
  tab
})