#' @include partition_class.R partitionBundle_class.R context_class.R contextBundle_class.R
#' @include comp_class.R
NULL

#' trim an object
#' 
#' Method to trim and adjust objects by 
#' applying thresholds, minimum frequencies etc. It can be applied to 'context',
#' 'comp', 'context', 'partition' and 'partitionBundle' objects.
#' 
#' @param object the object to be trimmed
#' @param minSignificance minimum significance level
#' @param minTotal the minimum frequency
#' @param maxRank maximum rank
#' @param posFilter exclude words with a POS tag not in this list
#' @param tokenFilter tokens to exclude from table
#' @param filterType either "include" or "exclude"
#' @param pAttribute character vector, either lemma or word
#' @param verbose whether to be talkative
#' @param drop partitionObjects you want to drop, specified either by number or name
#' @param minSize a minimum size for the partitions to be kept
#' @param keep specify names of partitions to keep, everything else is dropped
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
setMethod("trim", "compBundle", function(object, minSignificance=0, minFrequency=0, maxRank=0, tokenFilter=NULL, posFilter=NULL, filterType="include", mc=FALSE){
  rework <- new("compBundle")
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


#' @exportMethod trim
#' @docType methods
#' @rdname trim-method
setMethod("trim", "partitionBundle", function(object, pAttribute=NULL, minFrequency=0, posFilter=NULL,  tokenFilter=NULL, drop=NULL, minSize=0, keep=NULL, mc=NULL, ...){
  if (is.null(mc)) mc <- slot(get('session', '.GlobalEnv'), 'multicore')
  pimpedBundle <- object
  if (minFrequency !=0 || !is.null(posFilter) || !is.null(tokenFilter)){
    if (mc == TRUE) {
      pimpedBundle@objects <- mclapply(object@objects, function(x) trim(x, pAttribute=pAttribute, minFrequency=minFrequency, posFilter=posFilter, tokenFilter=tokenFilter))
    } else {
      pimpedBundle@objects <- lapply(object@objects, function(x) trim(x, pAttribute=pAttribute, minFrequency=minFrequency, posFilter=posFilter, tokenFilter=tokenFilter))    
    }
  }
  if (minSize >= 0){
    df <- data.frame(
      name=names(pimpedBundle),
      noToken=summary(pimpedBundle)$token,
      stringsAsFactors=FALSE
    )
    toKill <- subset(df, df$noToken < minSize)$name
    if (length(toKill) > 0) {drop <- c(toKill, drop)}
  }
  if (!is.null(drop)) {
    if (is.null(names(object@objects)) || any(is.na(names(object@objects)))) {
      warning("there a partitions to be dropped, but some or all partitions do not have a name, which may potentially cause errors or problems")
    }
    if (is.character(drop) == TRUE){
      pimpedBundle@objects[which(names(pimpedBundle@objects) %in% drop)] <- NULL
    } else if (is.numeric(drop == TRUE)){
      pimpedBundle@objects[drop] <- NULL
    }
  }
  if (!is.null(keep)){
    pimpedBundle@objects <- pimpedBundle@objects[which(names(pimpedBundle@objects) %in% keep)]
  }
  pimpedBundle
})


#' @importFrom Matrix rowSums
#' @importFrom tm stopwords
#' @importFrom slam as.simple_triplet_matrix
#' @rdname trim-method
setMethod("trim", "TermDocumentMatrix", function(object, minTotal=NULL, stopwords=NULL, keep=NULL, verbose=TRUE){
  # mat <- as.sparseMatrix(object)
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

#' shrink a matrix for topicmodelling
#' 
#' @param object an object of class \code{"DocumentTermMatrix"}
#' @param minCount minimum overall occurrence of a term in the matrix
#' @param minTfIdfMean minimum mean value for tf-idf (suggested: 0.005)
#' @param sparse will be passed into \code{"removeSparseTerms"} from \code{"tm"}-package
#' @param stopwordsLanguage e.g. "german", to get stopwords defined in the tm package
#' @param removeTrash if TRUE, terms containing special chars, numbers or length = 1 will be removed
#' @importFrom slam col_sums
setMethod("trim", "DocumentTermMatrix", function(object, minTotal=2, minTfIdfMean = 0.005, sparse=0.995, stopwordsLanguage="german", removeTrash=TRUE, verbose=TRUE){
  # min count
  if (!is.null(minTotal)){
    if (verbose == TRUE) message("... applying minTotal ", appendLF = FALSE)
    tfTotal <- slam::col_sums(object)
    tokensToKeep <- names(tfTotal[which(tfTotal >= minTotal)])
    if (verbose == TRUE) message("(reduction: ", ncol(object) - length(tokensToKeep), ")")
    object <- object[,tokensToKeep]
  }
  
  # min tf idf mean
  if (!is.null(minTfIdfMean)){
    if (verbose == TRUE) message("... applying minTfIdfMean ", appendLF = FALSE)
    dtmTfidf <- weigh(object, method="tfidf")
    tfidfMeans <- means(dtmTfidf, dim=1)
    tokensToKeep2 <- names(tfidfMeans[which(tfidfMeans > minTfIdfMean)])
    tokensToKeep2 <- tokensToKeep2[!is.na(tokensToKeep2)]
    if (verbose == TRUE) message("(reduction: ", ncol(object) - length(tokensToKeep2), ")")
    object <- object[,tokensToKeep2]
  }
  
  # reduce sparsity
  if (!is.null(sparse)){
    if (verbose == TRUE) message("... reducing sparsity ", appendLF = FALSE)
    ncolOld <- ncol(object)
    object <- tm::removeSparseTerms(object, sparse=sparse)
    if (verbose == TRUE) message("(reduction of: ", ncolOld - ncol(object), ")")
  }
  
  terms <- dimnames(object)[["Terms"]]
  
  if (verbose == TRUE) message("... filtering vocabulary ", appendLF = FALSE)
  termsToDrop <- list()
  if (!is.null(stopwordsLanguage)){
    termsToDrop[["stopwordsToDrop"]] <- which(terms %in% stopwords(stopwordsLanguage))
  }
  if (removeTrash == TRUE){
    termsToDrop[["specialChars"]] <- which(!c(1:length(terms)) %in% grep("^[a-zA-ZéäöüÄÖÜ-ß|-]+$", terms))
    termsToDrop[["numbers"]] <- grep("[0-9]", terms)
    termsToDrop[["stubs"]] <- which(nchar(terms) == 1)
  }
  
  # Entfernen von Sonderzeichen, Zahlen, Stopwoertern, Stummeln
  termsToDrop <- unique(unlist(termsToDrop))
  if (length(termsToDrop) > 0) {
    ncolOld <- ncol(object)
    object <- object[,-termsToDrop]
    if (verbose == TRUE) message("(reduction: ", ncolOld - ncol(object), ")")
  }
  
  attr(object, "weighting") <- c("term frequency", "tf")
  object
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

#' @exportMethod subset
#' @rdname cooccurrences-class
setMethod("trim", "cooccurrences", function(object, by=NULL){
  if (is.null(by) == FALSE){
    keys <- unlist(lapply(c("a", "b"), function(what) paste(what, object@pAttribute, sep="_")))
    setkeyv(by@stat, keys)
    setkeyv(object@stat, keys)
    object@stat <- by@stat[object@stat]
    object@stat <- object@stat[by@stat]
    for (toDrop in grep("i\\.", colnames(object@stat), value=T)) object@stat[, eval(toDrop) := NULL, with=TRUE]
    object@stat[, "count_ref" := NULL]
    object@stat[, "count_coi" := NULL]
  }
  object
})


# #' @rdname cooccurrences-class
# setMethod("trim", "cooccurrences", function(object, mc=TRUE, reshape=FALSE, by=NULL, ...){
#   if (reshape == TRUE) object <- .reshapeCooccurrences(object, mc=mc)
#   if (is.null(by) == FALSE){
#     if (class(by) %in% c("compCooccurrences", "cooccurrencesReshaped")){
#       bidirectional <- strsplit(rownames(by@stat), "<->")
#       fromTo <- c(
#         sapply(bidirectional, function(pair) paste(pair[1], "->", pair[2], sep="")),
#         sapply(bidirectional, function(pair) paste(pair[2], "->", pair[1], sep=""))
#       ) 
#       object@stat <- object@stat[which(rownames(object@stat) %in% fromTo),]
#     }
#   }
#   callNextMethod()
# })
# 

