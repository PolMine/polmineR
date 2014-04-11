#' crosstab class
#' 
#' class for keeping crosstabulations of frequencies of queries
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{partitions}:}{Object of class \code{"data.frame"} with sizes of the partition sizes for combinations of s-attributes analyzed }
#'     \item{\code{abs}:}{Object of class \code{"list"} for each query: a data frame with absolute frequencies }
#'     \item{\code{rel}:}{Object of class \code{"list"} for each query: a data frame with relative frequencies }
#'     \item{\code{total}:}{Object of class \code{"list"} with list elements "abs" and "rel" - all queries are summed up }
#'     \item{\code{rows}:}{Object of class \code{"character"} what you find in the rows }
#'     \item{\code{cols}:}{Object of class \code{"character"} what you find in the columns }
#'     \item{\code{query}:}{Object of class \code{"character"} the original queries }
#'   }
#'   
#' @section Methods:
#'    \describe{
#'     \item{show}{get summary of the object}
#'    }
#'     
#' @name crosstab-class
#' @aliases show,crosstab-method
#' @exportClass crosstab
#' @docType class
#' @rdname crosstab-class
setClass("crosstab",
         representation(partitions="data.frame", 
                        abs="list",
                        rel="list",
                        total="list",
                        rows="character",
                        cols="character",
                        query="character"
         )
)

.crosstabToken <- function(Partition,rows, cols, pAttribute, query){
  hits <- .queryCpos(query, Partition, pAttribute)
  sAttrRows <- paste(Partition@corpus,'.', rows, sep='')
  sAttrCols <- paste(Partition@corpus,'.', cols, sep='')
  rowsAttribute <- cqi_struc2str(sAttrRows, cqi_cpos2struc(sAttrRows,hits[,1]))
  colsAttribute <- cqi_struc2str(sAttrCols, cqi_cpos2struc(sAttrRows,hits[,1]))
  tab <- as.data.frame.matrix(table(data.frame(rows=rowsAttribute, cols=colsAttribute)))  
  tab
}

.crosstabSizes <- function(Partition, rows, cols){
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
#' @param queries a list with character vectors containing two elements, the
#' s-attribute and its value (only ASCII characters)
#' @param verbose whether updates shall be printed
#' @return returns a list
#' @author Andreas Blaette
#' @noRd
.crosstab <- function (Partition, rows, cols, pAttribute, queries, verbose=TRUE) {
  crosstab <- new("crosstab")
  if (verbose==TRUE) message("Starting the inquiry")
  if (verbose==TRUE) message("... getting the shares of words in sub-partitions")
  crosstab@partitions <- .crosstabSizes(Partition, rows, cols)
  for ( i in 1:length(queries) ) {
    if (verbose==TRUE) message ('... getting frequencies for "', queries[i],'"')
    crosstab@abs[[queries[i]]] <- .crosstabToken(Partition, rows, cols, pAttribute, queries[i])
    crosstab@abs[[queries[i]]] <- .map(crosstab@partitions, crosstab@abs[[queries[i]]])
    crosstab@abs[[queries[i]]][is.na(crosstab@abs[[queries[i]]])] <- 0
    crosstab@rel[[queries[i]]] <- crosstab@abs[[queries[i]]]/crosstab@partitions
    crosstab@rel[[queries[i]]][is.infinite(as.matrix(crosstab@rel[[queries[i]]]))] <- 0 
    crosstab@rel[[queries[i]]][is.nan(as.matrix(crosstab@rel[[queries[i]]]))] <- 0
  }
  if (verbose==TRUE) message("... cumulating frequencies")
  crosstab <- .crosstab.total(crosstab)
  if (verbose==TRUE) message("... calculating relative frequencies")
  crosstab <- .crosstab.rel(crosstab)
  crosstab@rows <- rows
  crosstab@cols <- cols
  crosstab@query <- queries
  colnames(crosstab@partitions) <- gsub('^X(.*?)', '\\1', colnames(crosstab@partitions))
  rownames(crosstab@partitions)[which(rownames(crosstab@partitions)=="")] <- 'VOID'
  crosstab
}

.crosstab.rel <- function(crosstab){
  crosstab@total$rel <- crosstab@total$abs/crosstab@partitions
  crosstab@total$rel[is.infinite(as.matrix(crosstab@total$rel))] <- 0 
  crosstab@total$rel[is.nan(as.matrix(crosstab@total$rel))] <- 0
  crosstab
}

.crosstab.total <- function(crosstab){
  crosstab@total$abs <- crosstab@abs[[1]]
  if (length(names(crosstab@abs))>1) {
    for ( i in c(2:length(names(crosstab@abs)))) crosstab@total$abs <- crosstab@total$abs + crosstab@abs[[i]]
  }  
  crosstab
}

#' merge cols
#'
#' @param object the object to be passed
#' @param ... further parameters
#' @author Andreas Blaette
#' @export
#' @docType methods
#' @rdname mergeColsRegex-methods
#' @noRd
setGeneric("mergeColsRegex", function(object,...){standardGeneric("mergeColsRegex")})

setGeneric("mergeCols", function(object,...){standardGeneric("mergeCols")})
setGeneric("filterCols", function(object,...){standardGeneric("filterCols")})

#' merge two columns in a crosstab object
#' 
#' The columns with absolute frequencies (partition size, frequencies for a
#' query) are merged, and the relative frequencies are recalculated.
#' 
#' @method mergeCols crosstab
#' @param object the partition object
#' @param colname.old1 the colname of the first column to be merged
#' @param colname.old2 the colname of the second column to be merged
#' @param colname.new the colname of the merged column
#' @return the returned crosstab object has a matrix with partition sizes,
#' absoute query frequencies and relative query frequencies, just as the input
#' @author Andreas Blaette
#' @docType methods
#' @aliases mergeCols mergeCols-crosstab-method
#' @rdname mergeCols-crosstab-method
#' @exportMethod mergeCols
setMethod('mergeCols','crosstab', 
function(object, colname.old1, colname.old2, colname.new) {
  object@partitions[,colname.old1] <- object@partitions[,colname.old1] + object@partitions[,colname.old2]
  colnames(object@partitions)[which(colnames(object@partitions)==colname.old1)] <- colname.new
  object@partitions <- .dropcols(object@partitions, colname.old2)
  for (query in object@query) {
    object@abs[[query]][,colname.old1] <- object@abs[[query]][,colname.old1] + object@abs[[query]][,colname.old2]
    colnames(object@abs[[query]])[which(colnames(object@abs[[query]])==colname.old1)] <- colname.new
    object@abs[[query]] <- object@abs[[query]][-grep(colname.old2, colnames(object@abs[[query]]))]
    object@rel[[query]] <- object@abs[[query]]/object@partitions
  }
  object <- .crosstab.total(object)
  object <- .crosstab.rel(object)
  object
})

#' merge columns that match a regex
#' 
#' Merge columns of a crosstab object that match a regular expression
#' 
#' @method mergeColsRegex crosstab
#' @param object the partition object
#' @param regex a regular expression
#' @param colname.new the colname of the merged column
#' @return a crosstab object has a matrix with partition sizes,
#' absoute query frequencies and relative query frequencies, just as the input
#' @docType methods
#' @aliases mergeColsRegex mergeColsRegex,crosstab-method 
#' @rdname mergeColsRegex-crosstab-method
#' @exportMethod mergeColsRegex
setMethod("mergeColsRegex", "crosstab",
function(object, regex, colname.new) {
  match <- grep(regex, colnames(object@partitions))
  message('...', length(match), 'columns to be merged')
  if (length(match)>1) {
    object@partitions <- cbind(object@partitions, rowSums(object@partitions[,match]))
    object@partitions <- .dropcols(object@partitions, regex)      
    colnames(object@partitions)[ncol(object@partitions)] <- colname.new
    for (name in names(object@abs)) {
      object@abs[[name]] <- cbind(object@abs[[name]], rowSums(object@abs[[name]][,match]))
      object@abs[[name]] <- .dropcols(object@abs[[name]], regex)      
      colnames(object@abs[[name]])[ncol(object@abs[[name]])] <- colname.new
    }
  } else if (length(match==1)) {
    object@partitions <- cbind(object@partitions, object@partitions[,match])
    object@partitions <- .dropcols(object@partitions, regex)      
    colnames(object@partitions)[ncol(object@partitions)] <- colname.new
    for (name in names(object@abs)) {
      object@abs[[name]] <- cbind(object@abs[[name]], object@abs[[name]][,match])
      object@abs[[name]] <- .dropcols(object@abs[[name]], regex)      
      colnames(object@abs[[name]])[ncol(object@abs[[name]])] <- colname.new
    }
  } else {
    object@partitions <- cbind(object@partitions, rep(0, times=nrow(object@partitions)))
    colnames(object@partitions)[ncol(object@partitions)] <- colname.new    
    for (name in names(object@abs)) {
      object@abs[[name]] <- cbind(object@abs[[name]], rep(0, times=nrow(object@abs[[name]])))
      colnames(object@abs[[name]])[ncol(object@abs[[name]])] <- colname.new
    }
  }
  object <- .crosstab.total(object)
  object <- .crosstab.rel(object)
  object
})


#' drop columns from a crosstab object
#' 
#' columns indicated in a character vector are either dropped or maintained,
#' depending on whether the vector is used as a stoplist or a list of columns
#' to be kept
#' 
#' @method filterCols crosstab
#' @param object the crosstab object
#' @param crosstab the crosstab object to be reworked
#' @param filter a character vector with colnames
#' @param what if "drops", cols is used as a stoplist, if "keep", itis a list with
#' the columns to be kept
#' @return you get a crosstab object with partition size, absolute and relative
#' frequencies
#' @author Andreas Blaette
#' @aliases filterCols filterCols-crosstab-method
#' @exportMethod filterCols
#' @rdname filterCols-crosstab-method
setMethod("filterCols", "crosstab",
function(object, filter, what="drop"){
  if (what=="drop"){
    object@partitions <- .dropcols(object@partitions, filter)
    slot(object, "abs") <- lapply(slot(object,"abs"), function(x).dropcols(x, filter))
    slot(object, "rel") <- lapply(slot(object,"rel"), function(x).dropcols(x, filter))
  } else if (what=="keep"){
    object@partitions <- object@partitions[,which(colnames(object@partitions) %in% filter)]
    slot(object, "abs") <- lapply(slot(object, "abs"), function(x) x[,which(colnames(x) %in% filter)])
    slot(object, "rel") <- lapply(slot(object, "rel"), function(x) x[,which(colnames(x) %in% filter)])
  }
  object <- .crosstab.total(object)
  object <- .crosstab.rel(object)
  object
})

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
  print(object@total$rel)
  cat('\n')
  print(object@total$abs)
})

.dropcols <- function(tab, colname) {
  drop <- grep(colname, colnames(tab))
  tab <- tab[-drop]
}

.map <- function(tableToMatch, tableToAdjust) {
  tableToAdjust <- merge(t(tableToMatch), t(tableToAdjust), by.x="row.names", by.y="row.names", all.x=TRUE, all.y=TRUE)
  tableToAdjust <- tableToAdjust[,grep('\\.y', colnames(tableToAdjust))]
  tableToAdjust <- t(tableToAdjust)
  rownames(tableToAdjust) <- sub('(.*?)\\.y', '\\1', rownames(tableToAdjust))
  rownames(tableToAdjust)[which(rownames(tableToAdjust)=="V1")] <- 'VOID'
  rownames(tableToMatch)[which(rownames(tableToMatch)=="")] <- 'VOID'
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
  query <- .adjustEncoding(query, part@encoding)
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

#' Distribution of a query or multiple queries
#' 
#' The function returns the frequencies of a query or a multiple queries
#' in sub-partitions defined by one or two dimensions. This is a wrapper function, so the output will depend
#' on the number of queries and dimensions provided. Note that metadata need
#' to be set up for the partition.
#' 
#' @param partition a partition object that will be queried
#' @param query a character vector containing one or multiple queries
#' @param dim a character vector of length 1 or 2 providing the sAttributes 
#' @param pAttribute the p-attribute that will be looked up, typically 'word'
#' or 'lemma'
#' @return depends on the input, as this is a wrapper function
#' @export
#' @examples
#' \dontrun{
#' bt <- partition("PLPRBTTXT", tf=FALSE) # term frequencies are not needed
#' dispersion(plpr, "Vorsorge", "text_year")
#' }
#' @author Andreas Blaette
#' @export dispersion
dispersion <- function(partition, query, dim, pAttribute=drillingControls$pAttribute){
  if ( is.null(names(partition@metadata))) warning("Metadata need to ne set up in partition")
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