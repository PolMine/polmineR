#'@include partition.R
NULL

#' partitionCluster class
#' 
#' A cluster of partition objects.
#' 
#' @section Slots: \describe{ \item{\code{partitions}:}{Object of class
#'   \code{"list"} the partitions making up the cluster } 
#'   \item{\code{corpus}:}{Object of class \code{"character"} the CWB corpus the
#'   partition is based on } \item{\code{sAttributesFixed}:}{Object of class
#'   \code{"list"} fixed sAttributes } \item{\code{encoding}:}{Object of class
#'   \code{"character"} encoding of the corpus } 
#'   \item{\code{explanation}:}{Object of class \code{"character"} an
#'   explanation of the partition } \item{\code{xml}:}{Object of class
#'   \code{"character"} whether the xml is flat or nested } }
#'   
#' @section Methods: \describe{ \item{show}{\code{signature(object = 
#'   "partitionCluster")}: Display essential information } 
#'   \item{addPos}{\code{signature(object="partitionCluster")}: add list with 
#'   most frequent pos for a token } 
#'   \item{tf}{\code{signature(object="partitionCluster")}: get term frequencies
#'   } \item{trim}{\code{signature(object="partitionCluster")}: trim a 
#'   partitionCluster object } \item{[}{get frequency of a query} \item{[[}{get 
#'   a partition within the cluster} \item{+}{\code{signature(object = 
#'   "partitionCluster")}: combine two partitionClusters into a new one } 
#'  \item{html}{bla} }
#'   
#' @aliases partitionCluster-class show,partitionCluster-method 
#'   [,partitionCluster-method 
#'   [[,partitionCluster-method as.DocumentTermMatrix,partitionCluster-method 
#'   as.matrix,partitionCluster-method 
#'   as.TermDocumentMatrix,partitionCluster-method merge,partitionCluster-method
#'   merge,partitionCluster-method 
#'   as.sparseMatrix,partitionCluster-Method as.sparseMatrix 
#'   +,partitionCluster-method names,partitionCluster-method 
#'   summary,partitionCluster-method 
#'   +,partitionCluster,ANY-method [,partitionCluster,ANY,ANY,ANY-method
#'   +,partitionCluster,partition-method
#'   +,partitionCluster,partitionCluster-method as.partitionCluster,list-method
#' @rdname partitionCluster-class
#' @name partitionCluster-class
#' @exportClass partitionCluster
#' @docType class
#' @author Andreas Blaette
setClass("partitionCluster",
         representation(partitions="list", 
                        corpus="character",
                        sAttributesFixed="list",
                        encoding="character",
                        explanation="character",
                        xml="character"
         )
)



#' Generate a list of partitions
#' 
#' A list of partition objects with fixed s-attributes and one variable
#' s-attribute is generated
#' 
#' If var is list(text_date=NULL) for instance, all values for the provided s-attribute
#' in the partition
#' defined by sAttributesFixed will be retrived and used for defining the
#' partitions.
#' While generally S4 methods are used in the driller package, the return is a S3 method.
#' The reasons is that the number of partitions kept in the cluster is not known before the initialization.
#' Setting multicore to TRUE will speed up things. Error handling is less benevolent, risk of overheating, no verbose output.
#' 
#' @param corpus the CWB corpus to be used
#' @param def a list with the definition of a partition that shall be prepared
#' @param var list indicating the s-attribute to be variabel
#' @param encoding encoding of the corpus, if not provided, encoding provided in the registry file will be used
#' @param tf the pAttributes for which term frequencies shall be retrieved
#' @param meta a character vector
#' @param method either 'grep' or 'in'
#' @param xml either 'flat' (default) or 'nested'
#' @param prefix a character vector that will serve as a prefix for partition labels
#' @return a S3 class 'partitionCluster', which is a list with partition objects
#' @importFrom parallel mclapply
#' @export partitionCluster
#' @aliases partitionCluster
#' @author Andreas Blaette
partitionCluster <- function(
  corpus, def, var, prefix=c(""),
  encoding=NULL, tf=c("word", "lemma"), meta=NULL, method="grep", xml="flat"
) {
  if (length(names(var))==1) {
    sAttributeVar <- names(var)
    sAttributeVarValues <- var[[sAttributeVar]]
  } else {
    warning("only one variable s-attribute may be provided")
  }
  multicore <- get("drillingControls", '.GlobalEnv')[['multicore']]
  multicoreMessage <- ifelse(
    multicore==TRUE,
    ' (use multicore: TRUE)',
    ' (use multicore: FALSE)'
  )
  message('\nPreparing cluster of partitions', multicoreMessage)
  cluster <- new("partitionCluster")
  cluster@corpus <- corpus
  cluster@sAttributesFixed <- def
  message('... setting up base partition')
  partitionBase <- partition(corpus, def, tf=c(), meta=meta, method=method, xml=xml, verbose=FALSE)
  cluster@encoding <- partitionBase@encoding
  if (is.null(sAttributeVarValues)){
    message('... getting values of fixed s-attributes')
    sAttributeVarValues <- unique(cqi_struc2str(paste(corpus, '.', sAttributeVar, sep=''), partitionBase@strucs))
    Encoding(sAttributeVarValues) <- cluster@encoding
    message('... number of partitions to be initialized: ', length(sAttributeVarValues))
  }
  if (multicore==FALSE) {
    for (sAttribute in sAttributeVarValues){
      sAttr <- list()
      sAttr[[sAttributeVar]] <- sAttribute
      cluster@partitions[[sAttribute]] <- zoom(partitionBase, def=sAttr, label=sAttribute, tf=tf)
    }
  } else if (multicore==TRUE) {
    message('... setting up the partitions')
    cluster@partitions <- mclapply(
      sAttributeVarValues,
      function(x) zoom(
        partitionBase,
        def=sapply(sAttributeVar, function(y) x, USE.NAMES=TRUE),
        label=x,
        tf=tf
      )
    )
  }
  names(cluster@partitions) <- paste(.adjustEncoding(prefix, cluster@encoding), sAttributeVarValues, sep='')
  cluster
}

#' Show method for partitionCluster Objects 
#'
#' Prints the number of partitions in the cluster and returns the respective sizes
#' 
#' @param object the partitionCluster object
#' @exportMethod show
#' @noRd
setMethod("show", "partitionCluster", function (object) {
  stat <- summary(object)
  cat('** PartitionCluster object: **\n')
  cat(sprintf('%-25s', 'Number of partitions:'), length(object@partitions), '\n')
  # same code as in show-method for partition
  sFix <- unlist(lapply(
    names(object@sAttributesFixed),
    function(x) {paste(x, "=", paste(object@sAttributesFixed[[x]], collapse="/"))}
  ))
  cat(sprintf("%-25s", "sAttributes Fixed:"), sFix[1], '\n')
  if (length(sFix)>1) {for (i in length(sFix)){cat(sprintf("%-25s", " "), sFix[i], '\n')}}
  cat("\n")
  print(stat, quote=FALSE)
})

#' Summary method for partitionCluster Objects 
#'
#' simple statistic 
#' 
#' @param object the partitionCluster object
#' @exportMethod summary
#' @noRd
setMethod("summary", "partitionCluster", function (object) {
  summary <- data.frame(
    partition=names(object@partitions),
    token=unlist(lapply(object@partitions, function(x) x@size)),
    stringsAsFactors=FALSE
   )
  pAttr <- unique(unlist(lapply(object@partitions, function(x) names(x@tf))))
  if (!is.null(pAttr)){
    raw <- lapply(pAttr, function(x) unlist(lapply(object@partitions, function(y) nrow(y@tf[[x]]))))
    raw <- do.call(data.frame, raw)
    colnames(raw) <- paste("unique_", pAttr, sep="")
    summary <- data.frame(summary, raw, stringsAsFactors=FALSE)
  }
  rownames(summary) <- c(1:nrow(summary))
  summary
})




#' Merge the partitions in a cluster into one partition
#' 
#' The partitions in a cluster object will be merged into one new partition
#' 
#' The function aggregates several partitions into one partition. The
#' prerequisite for this function to work properly is that there are no
#' overlaps of the different partitions that are to be summarized.
#' Encodings and the root node need to be identical, too.
#' 
#' @param object a cluster object
#' @param label the label for the new partition
#' @return An object of the class 'partition. See partition for the
#' details on the class.
#' @author Andreas Blaette
#' @exportMethod merge
#' @noRd
setMethod("merge", "partitionCluster", function(x, label=c("")){
  y <- new("partition")
  cat('There are', length(x@partitions), 'partitions to be merged\n')
  y@corpus <- unique(vapply(x@partitions, FUN.VALUE="characer", function(p) p@corpus))
  if (length(y@corpus) >  1) warning("WARNING: This function will not work correctly, as the cluster comprises different corpora")
  y@xml <- unique(vapply(x@partitions, function(p)p@xml, FUN.VALUE="character"))
  y@encoding <- unique(vapply(x@partitions, function(p)p@encoding, FUN.VALUE="character"))
  y@sAttributeStrucs <- unique(vapply(x@partitions, function(p) p@sAttributeStrucs, FUN.VALUE="character"))
  message('... merging the struc vectors')
  for (name in names(x@partitions)) {y@strucs <- union(y@strucs, x@partitions[[name]]@strucs)}
  message('... generating corpus positions')
  cpos <- data.matrix(t(data.frame(lapply(
    y@strucs,
    function(s){cqi_struc2cpos(paste(y@corpus,'.', y@sAttributeStrucs, sep=''),s)})
    )))
  rownames(cpos) <- NULL
  y@cpos <- cpos
  y@explanation=c(paste("this partition is a merger of the partitions", paste(names(x@partitions), collapse=', ')))
  y@label <- label
  y
})

#' @exportMethod [[
setMethod('[[', 'partitionCluster', function(x,i){
  return(x@partitions[[i]])
}
)

#' @exportMethod [
setMethod('[', 'partitionCluster', function(x,i){
  a <- unname(unlist(lapply(x@partitions, function(y) y@tf$word[i,2])))
  sizes <- unlist(lapply(x@partitions, function(y) y@size))
  dist <- data.frame(
    partition=names(x@partitions),
    tfAbs=a,
    tfRel=round(a/sizes*100000,2),
    row.names=c(1:length(x@partitions))
    )
  dist
}
)

#' Transform a partition cluster into a Term Document Matrix
#' 
#' Method based on the tm package, adds to as.TermDocumentMatrix
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionCluster object (S3 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param ... to make the check happy
#' @method as.TermDocumentMatrix partitionCluster
#' @return a TermDocumentMatrix
#' @author Andreas Blaette
#' @importFrom slam simple_triplet_matrix
#' @importFrom tm as.TermDocumentMatrix
#' @exportMethod as.TermDocumentMatrix
#' @noRd
setMethod("as.TermDocumentMatrix", "partitionCluster", function (x, pAttribute, ...) {
  encoding <- unique(unlist(lapply(x@partitions, function(c) c@encoding)))
  corpus <- unique(unlist(lapply(x@partitions, function(c) c@corpus)))
  i <- as.integer(unname(unlist(lapply(x@partitions,
                     function(c) {a <- c@tf[[pAttribute]][,1]
                                  a <- a+1
                                  a})
  )))
  j <- unlist(lapply(c(1:length(x@partitions)),
                     function(m) {rep(
                       m,times=nrow(x@partitions[[m]]@tf[[pAttribute]])
                     )
                     }
  ))
  v <- as.integer(unlist(lapply(x@partitions, function(c) c@tf[[pAttribute]][,2])))
  attr <- paste(corpus, '.', pAttribute, sep='')
  lexicon.size <- cqi_lexicon_size(attr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x@partitions),
                               nrow=lexicon.size+1,
                               dimnames=list(
                                 Terms=cqi_id2str(attr, c(0:lexicon.size)),
                                 Docs=names(x@partitions))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")  
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  mat
})


#' Turn a partition cluster into a matrix
#' 
#' Method based on the tm package.
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionCluster object (S3 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param ... necessary for S3 method?!
#' @method as.matrix partitionCluster
#' @return a matrix
#' @author Andreas Blaette
#' @exportMethod as.matrix
#' @noRd
setMethod("as.matrix", "partitionCluster", function(x, pAttribute, ...) {
  as.matrix(as.TermDocumentMatrix(x, pAttribute))
})

#' Turn a partition cluster into a document-term matrix
#' 
#' Method based on the tm package.
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionCluster object (S3 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param ... make R happy
#' @method as.DocumentTermMatrix partitionCluster
#' @return a DocumentTermMatrix
#' @author Andreas Blaette
#' @importFrom tm as.DocumentTermMatrix
#' @exportMethod as.DocumentTermMatrix
#' @noRd
setMethod("as.DocumentTermMatrix", "partitionCluster", function(x, pAttribute, ...) {
  as.DocumentTermMatrix(as.TermDocumentMatrix(x, pAttribute))
})


#' @import Matrix
#' @include generics.R
setMethod("as.sparseMatrix", "partitionCluster", function(x, pAttribute, ...){
  message("... converting partitionCluster to TermDocumentMatrix")
  tdm_stm <- as.TermDocumentMatrix(x, "word")
  message("... converting TermDocumentMatrix to Matrix")
  retval <-  sparseMatrix(i=tdm_stm$i,
                          j=tdm_stm$j,
                          x=tdm_stm$v,
                          dims=c(tdm_stm$nrow, tdm_stm$ncol),
                          dimnames = dimnames(tdm_stm),
                          giveCsparse = TRUE)
 retval
})

#' @exportMethod names
setMethod("names", "partitionCluster", function(x){
  names(x@partitions)
})

# '@include partition.R
#' @exportMethod +
setMethod("+", signature(e1="partitionCluster", e2="partitionCluster"), function(e1, e2){
  newPartition <- new("partitionCluster")
  newPartition@partitions <- c(e1@partitions, e2@partitions)
  corpus <- unique(e1@corpus, e2@corpus)
  encoding <- unique(e1@encoding, e2@encoding)
  explanation <- paste(e1@explanation, e2@explanation)
  xml <- "not available"
  newPartition
})

#' @exportMethod +
#' @include partition.R
setMethod("+", signature(e1="partitionCluster", e2="partition"), function(e1, e2){
  if (e1@corpus != e2@corpus) warning("Please be careful - partition is from a different CWB corpus")
  e1@partitions[[length(e1@partitions)+1]] <- e2
  names(e1@partitions)[length(e1@partitions)] <- e2@label
  e1
})


#'@include partition.R
#' @exportMethod as.partitionCluster
setMethod("as.partitionCluster", "partition", function(object){
 newCluster <- new("partitionCluster")
 newCluster@partitions[[1]] <- object
 names(newCluster@partitions)[1] <- object@label
 newCluster@corpus <- object@corpus
 newCluster@encoding <- object@encoding
 newCluster@explanation <- c("derived from a partition object")
 newCluster
})

setMethod("as.partitionCluster", "list", function(object, ...){
  if (!all(unlist(lapply(object, class))=="partition")) warning("all objects in list need to be partition objects")
  newCluster <- new("partitionCluster")
  newCluster@partitions <- object
  newCluster@corpus <- unique(unlist(lapply(newCluster@partitions, function(x) x@corpus)))
  newCluster@encoding <- unique(unlist(lapply(newCluster@partitions, function(x) x@encoding)))
  names(newCluster@partitions) <- vapply(newCluster@partitions, function(x) x@label, FUN.VALUE="character")
  newCluster
})

setMethod("html", "partitionCluster", function(object, filename=c(), type="debate"){
  markdown <- paste(lapply(object@partitions, function(p) .partition2markdown(p, type)), collapse="\n* * *\n")
  markdown <- paste(
    paste('## Excerpt from corpus', object@corpus, '\n* * *\n'),
    markdown,
    '\n* * *\n',
    collapse="\n")
  if (is.null(filename)) {
    htmlFile <- .markdown2tmpfile(markdown)
  } else {
    cat(markdown, file=filename)    
  }
  if (is.null(filename)) browseURL(htmlFile)
})

