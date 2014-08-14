#' @include generics.R
NULL

#' crosstab S4 class
#' 
#' Class for storing crosstabulations of frequencies of queries
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{partitions}:}{Object of class \code{"data.frame"} with sizes of the partition sizes for combinations of s-attributes analyzed }
#'     \item{\code{abs}:}{Object of class \code{"data.frame"} for each query: a data frame with absolute frequencies }
#'     \item{\code{rel}:}{Object of class \code{"data.frame"} for each query: a data frame with relative frequencies }
#'     \item{\code{rows}:}{Object of class \code{"character"} what you find in the rows }
#'     \item{\code{cols}:}{Object of class \code{"character"} what you find in the columns }
#'     \item{\code{query}:}{Object of class \code{"character"} the original queries }
#'   }
#'   
#' @section Methods:
#'    \describe{
#'     \item{show}{get summary of the object}
#'     \item{t}{change rows and columns}
#'    }
#'     
#' @name crosstab-class
#' @aliases show,crosstab-method t,crosstab-method
#' @seealso \code{dispersion}
#' @exportClass crosstab
#' @docType class
#' @rdname crosstab-class
setClass("crosstab",
         representation(partitions="data.frame", 
                        abs="data.frame",
                        rel="data.frame",
                        rows="character",
                        cols="character",
                        query="character"
         )
)


# documented with crosstab class
setMethod("t", "crosstab", function(x){
  x@partitions <- as.data.frame(t(x@partitions))
  x@abs <- as.data.frame(t(x@abs))
  x@rel <- as.data.frame(t(x@rel))
  rows <- x@rows
  cols <- x@cols
  x@rows <- cols
  x@cols <- rows
  x  
})

.crosstabToken <- function(Partition,rows, cols, pAttribute, query){
  hits <- .queryCpos(query, Partition, pAttribute)
  sAttrRows <- paste(Partition@corpus,'.', rows, sep='')
  sAttrCols <- paste(Partition@corpus,'.', cols, sep='')
  rowsAttribute <- cqi_struc2str(sAttrRows, cqi_cpos2struc(sAttrRows,hits[,1]))
  colsAttribute <- cqi_struc2str(sAttrCols, cqi_cpos2struc(sAttrRows,hits[,1]))
  tab <- as.data.frame.matrix(table(data.frame(rows=rowsAttribute, cols=colsAttribute)))  
  tab
}

#' Size of sub-partitions
#' @param Partition a partition object
#' @param rows what to find in rows
#' @param cols what to find in cols
#' @noRd
.crosstabulationSizes <- function(Partition, rows, cols){
  strucSize= Partition@cpos[,2] - Partition@cpos[,1] + 1
  sAttrRows <- paste(Partition@corpus,'.', rows, sep='')
  sAttrCols <- paste(Partition@corpus,'.', cols, sep='')
  tab <- data.frame(
    strucSize,
    rows=Partition@metadata$table[,rows],
    cols=Partition@metadata$table[,cols]
    )
  ctab <- xtabs(strucSize~rows+cols, data=tab)
  ctab <- data.frame(as.matrix(unclass(ctab)))
  colnames(ctab)[which(colnames(ctab)=="NA.")] <- "NA"
  rownames(ctab)[which(colnames(ctab)=="NA.")] <- "NA"
  ctab
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
#' @param pAttribute the pattribute to look up
#' @param query a character vector
#' @param verbose whether updates shall be printed
#' @return returns a list
#' @author Andreas Blaette
#' @noRd
.crosstab <- function (Partition, rows, cols, pAttribute, query, verbose=TRUE) {
  crosstab <- new("crosstab")
  if (length(Partition@metadata) == 0){
    Partition <- enrich(Partition, meta=c(rows, cols))
  } else if (length(Partition@metadata) > 0 && any(!(c(rows, cols) %in% colnames(Partition@metadata$table)))) {
    Partition <- enrich(Partition, meta=c(rows, cols))
  }
  if (verbose==TRUE) message("... getting the shares of words in sub-partitions")
  crosstab@partitions <- .crosstabulationSizes(Partition, rows, cols)
  if (verbose==TRUE) message ('... getting frequencies')
  crosstab@abs <- .crosstabToken(Partition, rows, cols, pAttribute, query)
  crosstab@abs <- .map(crosstab@partitions, crosstab@abs)
  crosstab@abs[is.na(crosstab@abs)] <- 0
  crosstab@rel <- crosstab@abs/crosstab@partitions
  crosstab@rel[is.infinite(as.matrix(crosstab@rel))] <- 0 
  crosstab@rel[is.nan(as.matrix(crosstab@rel))] <- 0
  crosstab@rows <- rows
  crosstab@cols <- cols
  crosstab@query <- query
  colnames(crosstab@partitions) <- gsub('^X(.*?)', '\\1', colnames(crosstab@partitions))
  rownames(crosstab@partitions)[which(rownames(crosstab@partitions)=="")] <- 'VOID'
  crosstab
}


#' merge two columns in a crosstab object
#' 
#' The columns with absolute frequencies (partition size, frequencies for a
#' query) are merged, and the relative frequencies are recalculated.
#' 
#' @method mergeCols crosstab
#' @param object the partition object
#' @param colnameOld1 the colname of the first column to be merged
#' @param colnameOld2 the colname of the second column to be merged
#' @param colnameNew the colname of the merged column
#' @return the returned crosstab object has a matrix with partition sizes,
#' absoute query frequencies and relative query frequencies, just as the input
#' @noRd
.crosstabMergeCols <- function(object, colnameOld1, colnameOld2, colnameNew) {
  object@partitions[,colnameOld1] <- object@partitions[,colnameOld1] + object@partitions[,colnameOld2]
  colnames(object@partitions)[which(colnames(object@partitions)==colnameOld1)] <- colnameNew
  object@partitions <- .dropcols(object@partitions, colnameOld2)
  object@abs[,colnameOld1] <- object@abs[,colnameOld1] + object@abs[,colnameOld2]
  colnames(object@abs)[which(colnames(object@abs)==colnameOld1)] <- colnameNew
  object@abs <- object@abs[-grep(colnameOld2, colnames(object@abs))]
  object@rel <- object@abs/object@partitions
  object
}

#' merge columns that match a regex
#' 
#' Merge columns of a crosstab object that match a regular expression
#' 
#' @param object the partition object
#' @param regex a regular expression
#' @param colname.new the colname of the merged column
#' @return a crosstab object has a matrix with partition sizes,
#' absoute query frequencies and relative query frequencies, just as the input
#' @noRd
.crosstabMergeColsRegex <- function(object, regex, colname.new) {
  match <- grep(regex, colnames(object@partitions))
  message('...', length(match), 'columns to be merged')
  if (length(match)>1) {
    object@partitions <- cbind(object@partitions, rowSums(object@partitions[,match]))
    object@partitions <- .dropcols(object@partitions, regex)      
    colnames(object@partitions)[ncol(object@partitions)] <- colname.new
    object@abs <- cbind(object@abs, rowSums(object@abs[,match]))
    object@abs <- .dropcols(object@abs, regex)      
    colnames(object@abs)[ncol(object@abs)] <- colname.new
  } else if (length(match==1)) {
    object@partitions <- cbind(object@partitions, object@partitions[,match])
    object@partitions <- .dropcols(object@partitions, regex)      
    colnames(object@partitions)[ncol(object@partitions)] <- colname.new
    object@abs <- cbind(object@abs, object@abs[,match])
    object@abs <- .dropcols(object@abs, regex)      
    colnames(object@abs)[ncol(object@abs)] <- colname.new
  } else {
    object@partitions <- cbind(object@partitions, rep(0, times=nrow(object@partitions)))
    colnames(object@partitions)[ncol(object@partitions)] <- colname.new    
    object@abs <- cbind(object@abs, rep(0, times=nrow(object@abs)))
    colnames(object@abs)[ncol(object@abs)] <- colname.new
  }
  object@rel <- object@abs/object@partitions
  object
}


#' drop columns from a crosstab object
#' 
#' Columns indicated in a character vector are either dropped or maintained,
#' depending on whether the vector is used as a stoplist or a list of columns
#' to be kept
#' 
#' @param x the crosstab object
#' @param filter a character vector with colnames
#' @param what if "drop", cols is used as a stoplist, if "keep", itis a list with
#' the columns to be kept
#' @return you get a crosstab object with partition size, absolute and relative
#' frequencies
#' @noRd
.crosstabDrop <- function(x, filter, what="drop"){
  object <- x
  if (what=="drop"){
    object@partitions <- .dropcols(object@partitions, filter)
    object@abs <- .dropcols(object@abs, filter)
    object@rel <- .dropcols(object@rel, filter)
  } else if (what=="keep"){
    object@partitions <- object@partitions[,which(colnames(object@partitions) %in% filter)]
    object@abs <- object@abs[,which(colnames(object@abs) %in% filter)]
    object@rel <- object@rel[,which(colnames(object@rel) %in% filter)]
  }
  object
}

#' show a crosstab object
#' 
#' @param object a crosstab object
#' @author Andreas Blaette
#' @rdname show-crosstab-method
#' @exportMethod show
#' @noRd
setMethod("show", "crosstab",
function(object){
  cat('Query:', object@query, '; Rows:', object@rows, '; Columns:', object@cols, '\n\n')
  print(object@rel)
  cat('\n')
  print(object@abs)
})

.dropcols <- function(tab, colname) {
  drop <- grep(colname, colnames(tab))
  tab <- tab[-drop]
  tab
}

.map <- function(tableToMatch, tableToAdjust) {
  colnames(tableToMatch) <- sub('X(\\d)', '\\1', colnames(tableToMatch))
  rownames(tableToAdjust)[which(rownames(tableToAdjust)=="")] <- 'VOID'
  rownames(tableToMatch)[which(rownames(tableToMatch)=="")] <- 'VOID'
  colnames(tableToAdjust)[which(colnames(tableToAdjust)=="V1")] <- 'VOID'
  colnames(tableToMatch)[which(colnames(tableToMatch)=="V1")] <- 'VOID'  
  tableToAdjust <- merge(t(tableToMatch), t(tableToAdjust), by.x="row.names", by.y="row.names", all.x=TRUE, all.y=TRUE)
  tableToAdjust <- tableToAdjust[,(nrow(tableToMatch)+2):ncol(tableToAdjust)]
  tableToAdjust <- t(tableToAdjust)
  rownames(tableToAdjust) <- sub('(.*?)\\.y', '\\1', rownames(tableToAdjust))
  tableToAdjust <- merge(tableToMatch, tableToAdjust, by.x="row.names", by.y="row.names", all.x=TRUE, all.y=TRUE)
  tableToAdjust <- tableToAdjust[,grep('V\\d+', colnames(tableToAdjust))]
  dimnames(tableToAdjust) <- dimnames(tableToMatch)
  colnames(tableToAdjust) <- gsub('^X(.*?)', '\\1', colnames(tableToAdjust))
  tableToAdjust
}




#' Distribution of hits
#' 
#' Based on a context object, get distribution of hits
#' 
#' To be used with context objects
#' 
#' @param ContextObject a context object
#' @param sAttribute the s-Attribute
#' @return a table
#' @author Andreas Blaette
#' @noRd
.distribution <- function(ContextObject, sAttribute){
  attribute <- paste(ContextObject@corpus, '.', sAttribute, sep='')
  distribution <- table(cqi_struc2str(attribute, cqi_cpos2struc(attribute, ContextObject@cpos[,1])))
  distribution
}


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
.queryDistribution <- function (part, pAttribute, query, sAttribute, rel=TRUE) {
  cpos <- .queryCpos(query, part, pAttribute)[,1]
  if(!is.null(cpos)){
    sAttr <- paste(part@corpus,'.',sAttribute, sep='')
    struc <- cqi_cpos2struc(sAttr, cpos)
    abs <- table(cqi_struc2str(sAttr,struc))
    if (rel==TRUE) {
      sizes <- xtabs(
        length~meta,
        data=data.frame(length=part@cpos[,2]-part@cpos[,1], meta=part@metadata$table[,sAttribute])
        )
      tf <- join(
        data.frame(sAttr=names(sizes), sizes=as.vector(sizes)),
        data.frame(sAttr=names(abs), abs=as.vector(abs)),
        by="sAttr", type="left"
        )
      tf <- cbind(tf, rel=tf$abs/tf$sizes)
      rownames(tf) <- tf[,1]
      tf <- tf[,c("abs", "rel", "sizes")]
      tf[is.na(tf)] <- 0
    } else {
      tf <- abs
    }
  } else {
    warning("no hits for query ", query)
    tf <- NULL
  }
  tf
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
.queriesDistribution <- function(part, pAttribute, queries, sAttribute){
  message("retrieving frequencies for the ", length(queries), " queries given")
  dist <- list()
  dist$subcorpussizes <- xtabs(
    length~meta,
    data=data.frame(length=part@cpos[,2]-part@cpos[,1], meta=part@metadata$table[,sAttribute])
    )
  abs <- as.data.frame(dist$subcorpussizes)
  for (query in queries) {
    message('... query: ', query)
    incoming <- .queryDistribution(part, pAttribute, query, sAttribute, rel=FALSE)
    if (!is.null(incoming)){
      abs <- merge(abs, as.data.frame(incoming), by.x="meta", by.y="Var1", all=TRUE)
    } else {
      abs <- cbind(abs, new=rep(0,times=nrow(abs)))
    }
    colnames(abs)[dim(abs)[2]] <- query
  }
  rownames(abs) <- abs[,"meta"]
  abs <- abs[,3:dim(abs)[2]]
  abs[is.na(abs)] <- 0
  colnames(abs) <- queries
  Encoding(rownames(abs)) <- part@encoding
  dist$abs <- t(abs)
  dist$rel <- apply(dist$abs, 1, function(x)x/dist$subcorpussizes)
  dist$rel <- t(dist$rel)
  Encoding(colnames(dist$rel)) <- part@encoding
  dist
}

#' Dispersion of a query or multiple queries
#' 
#' The function returns the frequencies of a query or a multiple queries
#' in sub-partitions defined by one or two dimensions. This is a wrapper function, so the output will depend
#' on the number of queries and dimensions provided.
#' 
#' @param partition a partition object that will be queried
#' @param query a character vector containing one or multiple queries
#' @param dim a character vector of length 1 or 2 providing the sAttributes 
#' @param pAttribute the p-attribute that will be looked up, typically 'word'
#' or 'lemma'
#' @return depends on the input, as this is a wrapper function
#' @seealso \code{crosstab-class}
#' @export
#' @examples
#' test <- partition("PLPRBTTXT", def=list(text_date=".*"), tf="word")
#' dispersion(test, "Integration", c("text_date"))
#' dispersion(test, "Integration", c("text_date", "text_party"))
#' dispersion(test, '"Integration.*"', c("text_date")) # note the brackets when using regex!
#' @author Andreas Blaette
#' @export dispersion
dispersion <- function(partition, query, dim, pAttribute=drillingControls$pAttribute){
  if ( is.null(names(partition@metadata))) {
    message("... required metadata missing, fixing this")
    partition <- enrich(partition, meta=dim)
  }
  if (class(query) == "cqpQuery"){
    query <- query@query
  }
  if (length(dim)==1){
    if (length(query)==1){
      result <- .queryDistribution(partition, pAttribute, query, dim)
    } else if (length(query)>1){
      result <- .queriesDistribution(partition, pAttribute, query, dim)
    }
  } else if (length(dim)==2){
    result <- .crosstab(partition, dim[1], dim[2], pAttribute, query)
  }
  result
}

