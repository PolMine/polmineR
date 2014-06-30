#' @include generics.R context.R
NULL


#' trim context object
#' 
#' Trim a context object by applying different filters.
#' 
#' @param object a context object to be filtered
#' @param minSignificance minimum significance level
#' @param minFrequency the minimum frequency
#' @param maxRank maximum rank
#' @param posFilter exclude words with a POS tag not in this list
#' @param tokenFilter tokens to exclude from table
#' @return context object
#' @author Andreas Blaette
#' @exportMethod trim
#' @aliases trim,context-method
#' @docType methods
#' @rdname trim-context-method
setMethod("trim", "context", function(object, minSignificance=0, minFrequency=0, maxRank=0, posFilter=NULL, tokenFilter=NULL){
  test <- object@statisticalTest
  if (maxRank==0) maxRank=nrow(object@stat)
  object@stat <- object@stat[order(object@stat[,test], decreasing=TRUE),]
  object@stat <- object@stat[which(object@stat[,test]>=minSignificance),]
  object@stat <- object@stat[which(object@stat[,"countCoi"]>=minFrequency),]
  object@stat[,"rank"] <- c(1:length(object@stat[,"rank"]))
  object@stat <- object@stat[which(object@stat[,"rank"]<=maxRank),]
  if (!is.null(tokenFilter)){
    object@stat <- object@stat[!rownames(object@stat) %in% tokenFilter,]
  }
  if (!is.null(posFilter)) {
    if(is.element("pos", colnames(object@stat))==FALSE){
      cat('... adding part-of-speech tags to statistics-table (may take a while)\n')
      object <- .addPos(object)      
    }
    object@stat<- object@stat[which(object@stat[,"pos"] %in% posFilter),]
  }
  object
})

#' trim keyness object
#' 
#' Trim a context object by applying different filters.
#' 
#' Maybe it would be more efficient to use the subset function.-
#' 
#' @param object a keyness object 
#' @param minSignificance minimum significance level
#' @param minFrequency the minimum frequency
#' @param maxRank maximum rank
#' @param tokenFilter tokens to exclude from table
#' @return a keyness object
#' @author Andreas Blaette
#' @aliases trim,keyness-method
#' @exportMethod trim
setMethod("trim", "keyness", function(object, minSignificance=0, minFrequency=0, maxRank=0, tokenFilter=NULL){
  test <- object@statisticalTest
  if (maxRank==0) maxRank <- nrow(object@stat)
  if (maxRank > nrow(object@stat)) maxRank <- nrow(object@stat)
  object@stat <- object@stat[order(object@stat[,test], decreasing=TRUE),]
  object@stat <- object@stat[which(object@stat[,test]>=minSignificance),]
  object@stat <- object@stat[which(object@stat[,"countCoi"]>=minFrequency),]
  if (!is.null(tokenFilter)){
    object@stat <- object@stat[!rownames(object@stat) %in% tokenFilter,]
  }
  object@stat[,"rank"] <- c(1:length(object@stat[,"rank"]))
  object@stat <- object@stat[which(object@stat[,"rank"]<=maxRank),]
  object
})

#' trim partition object
#' 
#' Trim, filter and adjust the tf slot of a partition object for the p-attribute specified.
#' You can drop the tf values if below a minimum frequency or token that do not correspond to a
#' a part-of-speech specified.
#' 
#' @param object a partition class object
#' @param pAttribute character vector, either lemma or word
#' @param minFrequency an integer
#' @param posFilter a character vector
#' @param ... further arguments
#' @return a trimmed partition object
#' @author Andreas Blaette
#' @aliases trim,partition-method
#' @docType methods
#' @exportMethod trim
#' @rdname trim-partition-method
setMethod("trim", "partition", function(object, pAttribute, minFrequency=0, posFilter=c(),  ...){
  new <- object
  if (length(pAttribute) > 1) warning("taking only one pAttribute at a time")
  message("Trimming partition ", new@label)
  if (!is.null(posFilter)) {
    if (! pAttribute %in% names(object@pos) ){
      message("... pos need to be added first")
      new <- addPos(new, pAttribute)
    }
    new@pos[[pAttribute]] <- new@pos[[pAttribute]][new@pos[[pAttribute]] %in% posFilter]
    new@tf[[pAttribute]] <- new@tf[[pAttribute]][rownames(new@tf[[pAttribute]]) %in% names(new@pos[[pAttribute]]),]
  }
  if (minFrequency > 0){
    new@tf[[pAttribute]] <- subset(new@tf[[pAttribute]], tf >= minFrequency)
    new@pos[[pAttribute]] <- new@pos[[pAttribute]][names(new@pos[[pAttribute]]) %in% rownames(new@tf[[pAttribute]])]
  }
  new 
})


#' trim partitionCluster Object
#' 
#' Apply the trim method to all partitions in a partitionCluster. In addition,
#' partitions can be dropped.
#' 
#' @param object a partitionCluster object
#' @param pAttribute character vector: "word", "lemma", or both
#' @param minFrequency minimum frequency of tokens
#' @param posFilter pos to keep
#' @param drop partitionObjects you want to drop, specified either by number or by label
#' @param minSize a minimum size for the partitions to be kept
#' @param keep specify labels of partitions to keep, everything else is dropped
#' @param ... further arguments (unused)
#' @return partitionCluster
#' @aliases trim,partitionCluster-method
#' @exportMethod trim
#' @rdname trim-partitionCluster-method
setMethod("trim", "partitionCluster", function(object, pAttribute=NULL, minFrequency=0, posFilter=c(),  drop=NULL, minSize=0, keep=NULL, ...){
  pimpedCluster <- object
  if (minFrequency !=0 || !is.null(posFilter)){
    if (get('drillingControls', '.GlobalEnv')[['multicore']] == TRUE) {
      pimpedCluster@partitions <- mclapply(object@partitions, function(x) trim(x, pAttribute, minFrequency, posFilter))
    } else {
      pimpedCluster@partitions <- lapply(object@partitions, function(x) trim(x, pAttribute, minFrequency, posFilter))    
    }
  }
  if (minSize > 0){
    toKill <- subset(
      data.frame(
        name=names(pimpedCluster),
        noToken=summary(pimpedCluster)$token,
        stringsAsFactors=FALSE
      ), noToken < minSize)$name
    if (length(toKill) > 0) {drop <- c(toKill, drop)}
  }
  if (!is.null(drop)) {
    if (is.null(names(object@partitions)) || any(is.na(names(object@partitions)))) {
      warning("there a partitions to be dropped, but some or all partitions do not have a label, which may potentially cause errors or problems")
    }
    if (is.character(drop) == TRUE){
      pimpedCluster@partitions[which(names(pimpedCluster@partitions) %in% drop)] <- NULL
    } else if (is.numeric(drop == TRUE)){
      pimpedCluster@partitions[drop] <- NULL
    }
  }
  if (!is.null(keep)){
    pimpedCluster@partitions <- pimpedCluster@partitions[which(names(pimpedCluster@partitions) %in% keep)]
  }
  pimpedCluster
})

#' trim crosstab object
#' 
#' Drop unwanted columns in a crosstab object, and merge columns by either explicitly stating the columns,
#' or providing a regex. If merge$old is length 1, it is assumed that a regex is provided
#' 
#' @param object a crosstab object to be adjusted
#' @param drop defaults to NULL, or a character vector giving columns to be dropped 
#' @param merge a list giving columns to be merged or exactly one string with a regex (see examples)
#' @return a modified crosstab object
#' @docType methods
#' @aliases trim,crosstab-method
#' @rdname trim-crosstab-method
#' @exportMethod trim
setMethod("trim", "crosstab", function(object, drop=NULL, merge=list(old=c(), new=c())){
  if (!is.null(drop)){
    object <- .crosstabDrop(x=object, filter=drop, what="drop")
  }
  if (!all(sapply(merge, is.null))){
    if (length(merge$new) != 1) warning("check length of character vectors in merge-list (needs to be 1)")
    if (length(merge$old) == 2){
      object <- .crosstabMergeCols(
        object,
        colnameOld1=merge$old[1], colnameOld2=merge$old[2],
        colnameNew=merge$new[1]
        )
    } else if (length(merge$old == 1 )){
      object <- .crosstabMergeColsRegex(object, regex=merge$old[1], colname.new=merge$new[1])
    } else {
      warning("length of merge$old not valid")
    }
  }
})