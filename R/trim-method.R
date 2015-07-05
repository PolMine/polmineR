#' @include partition-class.R partitionCluster-class.R context-class.R contextCluster-class.R
#' @include keyness-class.R keynessCluster-class.R
NULL

#' trim an object
#' 
#' Method that includes varying to adjust objects from the driller package by 
#' applying thresholds, minimum frequencies etc. It can be applied to 'context',
#' 'keyness', 'context', 'partition' and 'partitionCluster' objects. See 
#' the respective documentation:
#' \describe{
#'  \item{context:}{\code{method?trim("context")}}
#'  \item{keyness:}{\code{method?trim("keyness")}} 
#'  \item{partition:}{\code{method?trim("partition")}}
#'  \item{partitionCluster:}{\code{method?trim("partitionCluster")}} 
#'  \item{crosstab:}{\code{method?trim("crosstab")}}
#' }
#' 
#' @param object the object to be trimmed
#' @param ... further arguments
#' @author Andreas Blaette
#' @docType methods
#' @aliases trim trim-method trim,TermDocumentMatrix-method
#' @rdname trim-method
setGeneric("trim", function(object, ...){standardGeneric("trim")})


#' trim textstat object
#' 
#' @param cutoff a list with colnames and cutoff levels
#' @return a trimmed object
#' @docType methods
#' @noRd
setMethod("trim", "textstat", function(object, cutoff=NULL, ...){
  if (!is.null(cutoff)){
    # ensure that colnames provided are actually available
    if (!all(names(cutoff) %in% colnames(object@stat))){
      warning("PLEASE CHECK: one or more of the colnames provided in the cutoff-list are not available")
    }
    nrowBeforeCut <- nrow(object@stat)
    for (cut in names(cutoff)){
      object@stat <- object@stat[which(object@stat[,cut] >= cutoff[[cut]]),] 
    }
    message("... cutoff levels have been applied - ", (nrowBeforeCut - nrow(object@stat)), " rows have been dropped")
  }
  return(object)
})

#' trim context object
#' 
#' Trim a context object by applying different filters.
#' 
#' @param object a context object to be filtered
#' @param minSignificance minimum significance level
#' @param minFrequency the minimum frequency
#' @param maxRank maximum rank
#' @param rankBy a character vector indicating the column for ranking
#' @param posFilter exclude words with a POS tag not in this list
#' @param tokenFilter tokens to exclude from table
#' @return context object
#' @author Andreas Blaette
#' @exportMethod trim
#' @aliases trim,context-method
#' @docType methods
#' @rdname trim-context-method
setMethod("trim", "context", function(object, minSignificance=0, minFrequency=0, maxRank=0, rankBy=NULL, posFilter=NULL, tokenFilter=NULL){
  if (is.null(rankBy)) {
    test <- object@statisticalTest[1]  
  } else {
    test <- rankBy
  }
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
#' @param rankBy the column of the statistics-table to use for ordering
#' @param tokenFilter tokens to exclude from table
#' @param posFilter pos to keep
#' @param filterType either "include" or "exclude"
#' @param digits a list
#' @param verbose whether to be talkative
#' @return a keyness object
#' @author Andreas Blaette
#' @aliases trim,keyness-method
#' @exportMethod trim
#' @docType methods
setMethod("trim", "keyness", function(object, minSignificance=NULL, minFrequency=0, maxRank=0, rankBy=NULL, tokenFilter=NULL, posFilter=NULL, filterType="include", digits=NULL, verbose=TRUE){
  if (maxRank==0) maxRank <- nrow(object@stat)
  if (maxRank > nrow(object@stat)) maxRank <- nrow(object@stat)
  if (is.null(rankBy)) {
    test <- object@statisticalTest[1]
  } else {
    test <- rankBy
  }
  if (verbose == TRUE) message("... ordering results by ", test)
  object@stat <- object@stat[order(object@stat[,test], decreasing=TRUE),]
  if (!is.null(minSignificance)) object@stat <- object@stat[which(object@stat[,test]>=minSignificance),]
  object@stat <- object@stat[which(object@stat[,"countCoi"]>=minFrequency),]
  if (!is.null(tokenFilter)){
    object@stat <- object@stat[.filter[[filterType]](rownames(object@stat), tokenFilter),]
  }
  if (!is.null(posFilter)){
    object@stat <- object@stat[.filter[[filterType]](object@stat[, "pos"], posFilter),]
  }
  if (!is.null(digits)){
    for (n in names(digits)){
      object@stat[,n] <- round(object@stat[,n], digits[[n]])
    }
  }
  object@stat[,"rank"] <- c(1:length(object@stat[,"rank"]))
  object@stat <- object@stat[which(object@stat[,"rank"]<=maxRank),]
  object
})

#' @docType methods
#' @noRd
setMethod("trim", "keynessCluster", function(object, minSignificance=0, minFrequency=0, maxRank=0, tokenFilter=NULL, posFilter=NULL, filterType="include", mc=FALSE){
  rework <- new("keynessCluster")
  .trimFunction <- function(x) {
    trim( x, minSignificance=minSignificance, minFrequency=minFrequency, maxRank=maxRank,
    tokenFilter=tokenFilter, posFilter=posFilter, filterType=filterType)
  }
  if (mc == FALSE){
    rework@objects <- lapply(setNames(object@objects, names(object@objects)), function(x) .trimFunction(x))   
  } else if (mc == TRUE){
    rework@objects <- mclapply(setNames(object@objects, names(object@objects)), function(x) .trimFunction(x))  
  }
  rework
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
#' @param tokenFilter a character vector with tokens to keep in the tf list specified by pAttribute
#' @param ... further arguments
#' @return a trimmed partition object
#' @author Andreas Blaette
#' @aliases trim,partition-method
#' @docType methods
#' @exportMethod trim
#' @rdname trim-partition-method
setMethod("trim", "partition", function(object, pAttribute, minFrequency=0, posFilter=NULL,  tokenFilter=NULL, ...){
  rework <- object
  if (length(pAttribute) > 1) warning("taking only one pAttribute at a time")
  message("Trimming partition ", rework@label)
  if (!is.null(posFilter)) {
    if (! pAttribute %in% names(object@pos) ){
      message("... pos need to be added first")
      rework <- addPos(rework, pAttribute)
    }
    rework@pos[[pAttribute]] <- rework@pos[[pAttribute]][rework@pos[[pAttribute]] %in% posFilter]
    rework@tf[[pAttribute]] <- rework@tf[[pAttribute]][rownames(rework@tf[[pAttribute]]) %in% names(rework@pos[[pAttribute]]),]
  }
  if (minFrequency > 0){
    rework@tf[[pAttribute]] <- rework@tf[[pAttribute]][which(rework@tf[[pAttribute]][,"tf"] >= minFrequency),]
    rework@pos[[pAttribute]] <- rework@pos[[pAttribute]][names(rework@pos[[pAttribute]]) %in% rownames(rework@tf[[pAttribute]])]
  }
  if(!is.null(tokenFilter)) {
    tokenFilter <- .adjustEncoding(tokenFilter, rework@encoding)
    rework@tf[[pAttribute]] <- rework@tf[[pAttribute]][which(rownames(rework@tf[[pAttribute]]) %in% tokenFilter),]
  }
  rework 
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
#' @param tokenFilter a character vector with tokens to keep in the tf list specified by pAttribute
#' @param drop partitionObjects you want to drop, specified either by number or by label
#' @param minSize a minimum size for the partitions to be kept
#' @param keep specify labels of partitions to keep, everything else is dropped
#' @param mc if not NULL logical - whether to use multicore parallelization
#' @param ... further arguments (unused)
#' @return partitionCluster
#' @aliases trim,partitionCluster-method
#' @exportMethod trim
#' @docType methods
#' @rdname trim-partitionCluster-method
setMethod("trim", "partitionCluster", function(object, pAttribute=NULL, minFrequency=0, posFilter=NULL,  tokenFilter=NULL, drop=NULL, minSize=0, keep=NULL, mc=NULL, ...){
  if (is.null(mc)) mc <- slot(get('session', '.GlobalEnv'), 'multicore')
  pimpedCluster <- object
  if (minFrequency !=0 || !is.null(posFilter) || !is.null(tokenFilter)){
    if (mc == TRUE) {
      pimpedCluster@objects <- mclapply(object@objects, function(x) trim(x, pAttribute=pAttribute, minFrequency=minFrequency, posFilter=posFilter, tokenFilter=tokenFilter))
    } else {
      pimpedCluster@objects <- lapply(object@objects, function(x) trim(x, pAttribute=pAttribute, minFrequency=minFrequency, posFilter=posFilter, tokenFilter=tokenFilter))    
    }
  }
  if (minSize >= 0){
    toKill <- subset(
      data.frame(
        name=names(pimpedCluster),
        noToken=summary(pimpedCluster)$token,
        stringsAsFactors=FALSE
      ), noToken < minSize)$name
    if (length(toKill) > 0) {drop <- c(toKill, drop)}
  }
  if (!is.null(drop)) {
    if (is.null(names(object@objects)) || any(is.na(names(object@objects)))) {
      warning("there a partitions to be dropped, but some or all partitions do not have a label, which may potentially cause errors or problems")
    }
    if (is.character(drop) == TRUE){
      pimpedCluster@objects[which(names(pimpedCluster@objects) %in% drop)] <- NULL
    } else if (is.numeric(drop == TRUE)){
      pimpedCluster@objects[drop] <- NULL
    }
  }
  if (!is.null(keep)){
    pimpedCluster@objects <- pimpedCluster@objects[which(names(pimpedCluster@objects) %in% keep)]
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
#' @docType methods
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


setMethod("trim", "collocations", function(object, mc=TRUE, reshape=FALSE, by=NULL, ...){
  if (reshape == TRUE) object <- .reshapeCollocations(object, mc=mc)
  if (is.null(by) == FALSE){
    if (class(by) %in% c("keynessCollocations", "collocationsReshaped")){
      bidirectional <- strsplit(rownames(by@stat), "<->")
      fromTo <- c(
        sapply(bidirectional, function(pair) paste(pair[1], "->", pair[2], sep="")),
        sapply(bidirectional, function(pair) paste(pair[2], "->", pair[1], sep=""))
      ) 
      object@stat <- object@stat[which(rownames(object@stat) %in% fromTo),]
    }
  }
  callNextMethod()
})

#' @importFrom Matrix rowSums
#' @importFrom tm stopwords
#' @importFrom slam as.simple_triplet_matrix
setMethod("trim", "TermDocumentMatrix", function(object, minFrequency=NULL, stopwords=NULL, keep=NULL, verbose=TRUE){
  mat <- as.sparseMatrix(object)
  if (!is.null(minFrequency)){
    if (verbose) message("... applying minimum frequency")
    aggregatedFrequencies <- rowSums(mat)
    mat <- mat[which(aggregatedFrequencies >= minFrequency),]
  }
  if (!is.null(keep)){
    if (verbose) message("... removing words apart from those to keep")
    mat <- mat[which(rownames(mat) %in% keep),]
  }
  if (!is.null(stopwords)){
    if (verbose) message("... removing stopwords")
    mat <- mat[which(!rownames(mat) %in% stopwords("German")), ]
  }
  retval <- as.simple_triplet_matrix(mat)
  class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  retval
})
