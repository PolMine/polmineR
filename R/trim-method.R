#' @include partition-class.R partitionCluster-class.R context-class.R contextCluster-class.R
#' @include keyness-class.R keynessCluster-class.R
NULL

#' trim an object
#' 
#' Method to trim and adjust objects by 
#' applying thresholds, minimum frequencies etc. It can be applied to 'context',
#' 'keyness', 'context', 'partition' and 'partitionCluster' objects.
#' 
#' @param object the object to be trimmed
#' @param cutoff a list with colnames and cutoff levels
#' @param minSignificance minimum significance level
#' @param minFrequency the minimum frequency
#' @param maxRank maximum rank
#' @param rankBy a character vector indicating the column for ranking
#' @param posFilter exclude words with a POS tag not in this list
#' @param tokenFilter tokens to exclude from table
#' @param filterType either "include" or "exclude"
#' @param digits a list
#' @param pAttribute character vector, either lemma or word
#' @param verbose whether to be talkative
#' @param drop partitionObjects you want to drop, specified either by number or by label
#' @param minSize a minimum size for the partitions to be kept
#' @param keep specify labels of partitions to keep, everything else is dropped
#' @param stopwords words/tokens to drop
#' @param mc if not NULL logical - whether to use multicore parallelization
#' @param ... further arguments
#' @author Andreas Blaette
#' @docType methods
#' @aliases trim trim-method trim,TermDocumentMatrix-method
#' @rdname trim-method
setGeneric("trim", function(object, ...){standardGeneric("trim")})


#' @docType methods
#' @rdname trim-method
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

#' @aliases trim,context-method
#' @docType methods
#' @rdname trim-method
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

#' @exportMethod trim
#' @rdname trim-method
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
#' @rdname trim-method
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

#' @docType methods
#' @exportMethod trim
#' @rdname trim-method
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


#' @exportMethod trim
#' @docType methods
#' @rdname trim-method
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
#' @rdname trim-method
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

#' trim dispersion object
#' 
#' Drop unwanted columns in a dispersion object, and merge columns by either explicitly stating the columns,
#' or providing a regex. If merge$old is length 1, it is assumed that a regex is provided
#' 
#' @param object a crosstab object to be adjusted
#' @param drop defaults to NULL, or a character vector giving columns to be dropped 
#' @param merge a list giving columns to be merged or exactly one string with a regex (see examples)
#' @return a modified crosstab object
#' @docType methods
#' @rdname dispersion-class
#' @exportMethod trim
#' @docType methods
setMethod("trim", "dispersion", function(object, drop=NULL, merge=list(old=c(), new=c())){
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

