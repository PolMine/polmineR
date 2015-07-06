#' @include partition-class.R
NULL

setGeneric("dispersion", function(object, ...){standardGeneric("dispersion")})


#' helper function for .distributionCrosstab
#' @noRd
.tfDispersion <- function(object, query, dim, pAttribute){
  hits <- cpos(object, query, pAttribute)
  sAttrRows <- paste(object@corpus,'.', dim[1], sep='')
  sAttrCols <- paste(object@corpus,'.', dim[2], sep='')
  rowsAttribute <- cqi_struc2str(sAttrRows, cqi_cpos2struc(sAttrRows, hits[,1]))
  colsAttribute <- cqi_struc2str(sAttrCols, cqi_cpos2struc(sAttrRows, hits[,1]))
  tabRaw <- table(data.frame(rows=rowsAttribute, cols=colsAttribute))
  tab <- as(tabRaw, "matrix")
  dimnames(tab) <- setNames(list(rownames(tab), colnames(tab)), dim)
  tab
}


#' Contingency tables with term frequencies
#' 
#' Generates a 'crosstab' object with partition sizes, absolute and relative
#' frequencies
#' 
#' The original intention for this function was to be able to generate time series in a fashion very similar to the google ngram viewer.
#' Applications for this function are much broader.
#' The function will retrieve frequencies from the corpus using cwb-scan-corpus.
#' It is not required that term frequencies have been set up for the partition that is supplied-
#' 
#' 
#' @param partition a partition object
#' @param rows character string, supplying the s-attribute for the rows of the contingency table that is to be produced
#' @param cols character stringwhat shall be displayed in the cols
#' @param pAttribute the p-attribute to look up
#' @param query a character vector
#' @param verbose whether updates shall be printed
#' @return returns a list
#' @author Andreas Blaette
#' @noRd
.distributionCrosstab <- function(object, query, pAttribute, rows, cols, verbose=TRUE) {
  dispObject <- new("dispersion", dim=c(rows, cols), query=query)
  if (length(object@metadata) == 0){
    object <- enrich(object, meta=c(rows, cols))
  } else if (length(object@metadata) > 0 && any(!(c(rows, cols) %in% colnames(object@metadata$table)))) {
    object <- enrich(object, meta=c(rows, cols))
  }
  if (verbose==TRUE) message("... getting the shares of words in sub-partitions")
  dispObject@sizes <- dissect(object, dim=c(rows, cols), verbose=FALSE)
  if (verbose==TRUE) message ('... getting frequencies')
  absRaw <- .tfDispersion(object, query, dim=c(rows, cols), pAttribute)
  dispObject@abs <- .mapMatrices(matrixToMatch=dispObject@sizes, matrixToAdjust=absRaw)
  dispObject@abs[is.na(dispObject@abs)] <- 0
  dispObject@rel <- dispObject@abs/dispObject@sizes
  dispObject@rel[is.infinite(as.matrix(dispObject@rel))] <- 0 
  dispObject@rel[is.nan(as.matrix(dispObject@rel))] <- 0
  colnames(dispObject@sizes) <- gsub('^X(.*?)', '\\1', colnames(dispObject@sizes))
  rownames(dispObject@sizes)[which(rownames(dispObject@sizes) == "")] <- 'NA'
  dispObject
}

#' Distribution of hits
#' 
#' Based on a context object, get distribution of hits
#' 
#' @rdname context-class
setMethod("dispersion", "context", function(object, sAttribute){
  sAttr <- paste(object@corpus, '.', sAttribute, sep='')
  table(cqi_struc2str(sAttr, cqi_cpos2struc(sAttr, ContextObject@cpos[,1])))
})


#' Number of occurences of a query in subcorpora
#' 
#' Perform frequency count of a query.
#' 
#' The function is designed for queries comprising a single word. Regular
#' expressions can be used (not tested yet). For multi-word-queries,
#' \code{multiword.distribution} can be used.
#' 
#' Queries may be single words or multi-word units. In the first case, the
#' query runs much quicker. For multi-word units, the tokens need to be
#' separated by a space. Usage of regular may differ between single words and
#' multi-word units. Generally, regular expressions can be used.  The is
#' required to be in UTF-8 and is translated to Latin-1, as this assumed to be
#' the encoding of the corpus.
#' 
#' @param partition a partition object that will be queried
#' @param p.attribute the p-attribute to look for, typically 'word' or 'lemma'
#' @param query a query (for one word/token) that may contain a regular
#' expression
#' @param s.attribute the s-attribute that will be used for differentiation
#' @return you get a table object
#' @author Andreas Blaette
#' @seealso multiword.distribution, queries.distribution
#' @noRd 
.queryDistribution <- function (object, query, pAttribute, sAttribute, rel=TRUE) {
  dispObject <- new("dispersion", dim=sAttribute, query=query)
  cpos <- cpos(object, query, pAttribute)[,1]
  if(!is.null(cpos)){
    sAttr <- paste(object@corpus,'.',sAttribute, sep='')
    struc <- cqi_cpos2struc(sAttr, cpos)
    abs <- table(cqi_struc2str(sAttr,struc))
    absMatrix <- as(abs, "matrix")
    dimnames(absMatrix) <- list(names(abs), "tf")
    names(dimnames(absMatrix)) <- c(sAttribute, "tf")
    dispObject@abs <- absMatrix
    if (rel==TRUE) {
      sizes <- xtabs(
        length~meta,
        data=data.frame(length=object@cpos[,2]-object@cpos[,1], meta=object@metadata$table[,sAttribute])
        )
      sizeMatrix <- as(sizes, "matrix")
      dimnames(sizeMatrix) <- setNames(list(names(sizes), "noToken"), c(sAttribute, "size"))
      dispObject@sizes <- sizeMatrix
      relDf <- join(
        data.frame(sAttr=names(sizes), sizes=as.vector(sizes)),
        data.frame(sAttr=names(abs), abs=as.vector(abs)),
        by="sAttr", type="left"
        )
      relDf <- cbind(relDf, rel=relDf$abs/relDf$sizes)
      relDf[is.na(relDf)] <- 0
      relMatrix <- matrix(data=relDf[,"rel"], ncol=1, dimnames=setNames(list(relDf$sAttr, "rel"), c(sAttribute, "rel")))
      dispObject@rel <- relMatrix
    }
  } else {
    warning("no hits for query ", query)
    dispObject <- NULL
  }
  dispObject
}


#' Cpos of a query in a partition
#' 
#' Get the corpus positions for a query in a partition
#' 
#' so far, the function only works for queries containing a single word. For
#' the encoding of the query see query.distribution.
#' 
#' @param partition a partition object that will be queried
#' @param p.attribute the p-attribute that will be looked up, typically 'word'
#' or 'lemma'
#' @param queries a character vector containing the queries
#' @param s.attribute the s-attribute to be used to create categories of the
#' result
#' @return the function returns a table with the queries in the rows and the subcorpora in
#' the columns
#' @author Andreas Blaette
#' @seealso .query.distribution, multiword.distribution
#' @noRd
.queriesDistribution <- function(object, queries, pAttribute, sAttribute, rel, mc, verbose){
  if (verbose == TRUE) message("... retrieving frequencies for the ", length(queries), " queries given")
  queriesUnique <- unique(queries)
  if (length(queriesUnique) != length(queries)) {
    warning("Please note: Not all queries are unique, this analysis will use only unique queries!")
    queries <- queriesUnique
  }
  if ("" %in% queries){
    warning("empty string as query, needs to be removed")
    queries <- queries[-which(queries == "")]
  }
  dispersionObject <- new("dispersion", dim=sAttribute)
  subsetsRaw <- xtabs(
    length~meta,
    data=data.frame(length=object@cpos[,2]-object@cpos[,1], meta=object@metadata$table[,sAttribute])
    )
  dispersionObject@sizes <- matrix(
    data=as.vector(subsetsRaw), ncol=1,
    dimnames=setNames(list(names(subsetsRaw), "subsetSize"), c(sAttribute, "size"))
    )
  if (mc == FALSE){
    queryHits <- lapply(
      setNames(queries, queries),
      function(query) {
        if (verbose == TRUE) message('... processing query: ', query)
        .queryDistribution(object, query, pAttribute, sAttribute, rel=FALSE)
      }
    )
  } else if (mc == TRUE) {
    if (verbose == TRUE) message("... getting counts from corpus (parallel processing)")
    queryHits <- mclapply(
      setNames(queries, queries),
      function(query) .queryDistribution(object, query, pAttribute, sAttribute, rel=FALSE),
      mc.cores=slot(get("session", ".GlobalEnv"), "cores")
    )
  }
  queryHitsNullLogical <- vapply(queryHits, is.null, FUN.VALUE=TRUE)
  queryHitsFail <- which(queryHitsNullLogical == TRUE)
  queryHitsSuccess <- which(queryHitsNullLogical == FALSE)
  queryHits2 <- queryHits[queryHitsSuccess]
  queryHits3 <- lapply(
    names(queryHits2),
    function(x) {
      data.frame(
        partition=rownames(queryHits2[[x]]@abs),
        query=x,
        no=as.vector(queryHits2[[x]]@abs)
        )
    })
  queryHits4 <- do.call(rbind, queryHits3)
  subcorpusToMerge <- data.frame(
    partition=rownames(dispersionObject@sizes),
    query="subcorpus_size",
    no=dispersionObject@sizes[,1]
    )
  mergedDF <- rbind(subcorpusToMerge, queryHits4)
  tabulatedDF <- xtabs(no~partition+query, data=mergedDF)
#   for (query in queries) {
#     if (verbose == TRUE) message("... adjusting data.frames")
#     # incoming <- .queryDistribution(part, pAttribute, query, sAttribute, rel=FALSE)
#     incoming <- queryHits[[query]]
#     if (!is.null(incoming)){
#       abs <- merge(abs, as.data.frame(incoming), by.x="meta", by.y="Var1", all=TRUE)
#     } else {
#       abs <- cbind(abs, new=rep(0,times=nrow(abs)))
#     }
#     colnames(abs)[dim(abs)[2]] <- query
#   }
#  rownames(abs) <- abs[,"meta"]
#  abs <- abs[,3:dim(abs)[2]]
#  abs[is.na(abs)] <- 0
#  colnames(abs) <- queries
#  Encoding(rownames(abs)) <- part@encoding
  tabulatedDF1 <- as.matrix(ftable(tabulatedDF))
  # rownames(tabulatedDF1) <- object@metadata$table[,sAttribute]
  tabulatedDF2 <- tabulatedDF1[,-which(colnames(tabulatedDF1) == "subcorpus_size")]
  zeroValues <- matrix(
    data=rep(0, times=length(queryHitsFail)*nrow(tabulatedDF2)),
    ncol=length(queryHitsFail),
    dimnames=list(rownames(tabulatedDF),names(queryHitsFail))
    )
  tabulatedDF3 <- cbind(tabulatedDF2, zeroValues)
  tabulatedDF4 <- tabulatedDF3[,queries]
  dispersionObject@abs <- t(tabulatedDF4)
  if (rel == TRUE){
    if (verbose == TRUE) message("... calculating relative frequencies")
    dispersionObject@rel <- t(apply(
      dispersionObject@abs, 1,
      function(x)x/as.vector(dispersionObject@sizes)
    ))
    Encoding(colnames(dispersionObject@rel)) <- object@encoding
  }
  dispersionObject
}

#' Dispersion of a query or multiple queries
#' 
#' The function returns the frequencies of a query or a multiple queries
#' in sub-partitions defined by one or two dimensions. This is a wrapper function, so the output will depend
#' on the number of queries and dimensions provided.
#' 
#' @param object a partition object
#' @param query a character vector containing one or multiple queries
#' @param dim a character vector of length 1 or 2 providing the sAttributes 
#' @param pAttribute the p-attribute that will be looked up, typically 'word'
#' or 'lemma'
#' @param rel logical, whether to calculate relative values
#' @param mc logical, whether to use multicore
#' @param verbose logical, whether to be verbose
#' @return depends on the input, as this is a wrapper function
#' @seealso \code{crosstab-class}
#' @exportMethod dispersion
#' @examples
#' test <- partition("PLPRBTTXT", def=list(text_lp="17"), tf=NULL)
#' dispersion(test, "Integration", pAttribute="word", dim=c("text_year"))
#' foo <- dispersion(test, "Integration", c("text_year", "text_party"))
#' dispersion(test, '"Integration.*"', c("text_year")) # note the brackets when using regex!
#' @seealso tf
#' @author Andreas Blaette
#' @docType methods
#' @exportMethod dispersion
#' @rdname dispersion-method
#' @name dispersion
#' @aliases dispersion dispersion-method dispersion,partition-method
setMethod("dispersion", "partition", function(object, query, dim, pAttribute=NULL, rel=TRUE, mc=FALSE, verbose=TRUE){
  if ( is.null(pAttribute) ) pAttribute <- slot(get("session", ".GlobalEnv"), "pAttribute")
  if ( is.null(names(object@metadata))) {
    if (verbose == TRUE) message("... required metadata missing, fixing this")
    object <- enrich(object, meta=dim)
  }
  if (class(query) == "cqpQuery"){
    query <- query@query
  }
  if (length(dim) == 1){
    if (length(query) == 1){
      result <- .queryDistribution(object, query=query, pAttribute=pAttribute, sAttribute=dim)
    } else if (length(query) > 1){
      result <- .queriesDistribution(object, query, pAttribute, dim, rel=rel, mc=mc, verbose=verbose)
    }
  } else if (length(dim)==2){
    result <- .distributionCrosstab(
      object, query=query, pAttribute=pAttribute, rows=dim[1], cols=dim[2], verbose=verbose
      )
  }
  result
})
