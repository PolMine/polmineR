#' get term frequencies
#' 
#' Method to obtain term frequencies for one or multiple terms or queries.
#' 
#' @param object either a partition or a partitionCluster object
#' @param token a character vector (one or multiple terms to be looked up)
#' @param method either "in", "grep" or "cqp" (defaults to "in")
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
#' test <- partition("PLPRBTTXT", list(text_date=".*"), tf=c("word", "lemma"), method="grep")
#' tf(test, "Wir") # get frequencies for one token
#' tf(test, c("Wir", "lassen", "uns")) # get frequencies for multiple tokens
#' tf(test, c("Zuwander.*", "Integration.*"), method="grep") # get frequencies using "grep"-method
#' tf("PLPRTXT", c("machen", "Integration"), "word")
setGeneric("tf", function(object, ...){standardGeneric("tf")})

#' @rdname tf-method
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


#' @rdname tf-method
setMethod("tf", "partitionCluster", function(object, token, method="in", pAttribute=NULL, rel=FALSE){
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
        token=token,
        tf(object@objects[[x]], token, method=method, pAttribute=pAttribute)
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

#' @rdname tf-method
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