#' partition class
#' 
#' In the 'driller'-package, the class 'partition' is the basis for most other operations.
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{label}:}{Object of class \code{"character"} a label that may be useful }
#'  \item{\code{corpus}:}{Object of class \code{"character"} the CWB corpus the partition is based on }
#'  \item{\code{encoding}:}{Object of class \code{"character"} encoding of the corpus }
#'  \item{\code{sAttributes}:}{Object of class \code{"list"} s-attributes specifying the partition }
#'  \item{\code{explanation}:}{Object of class \code{"character"} an explanation of the partition }
#'  \item{\code{cpos}:}{Object of class \code{"matrix"} corpus positions }
#'  \item{\code{pos}:}{Object of class \code{"list"} with tables "abs", "rel" and "max"}
#'  \item{\code{size}:}{Object of class \code{"numeric"} total size of the partition }
#'  \item{\code{metadata}:}{Object of class \code{"list"} metadata information }
#'  \item{\code{strucs}:}{Object of class \code{"numeric"} the strucs defining the partition }
#'  \item{\code{tf}:}{Object of class \code{"list"} term frequencies }
#'  \item{\code{xml}:}{Object of class \code{"character"} whether the xml is flat or nested }
#'  \item{\code{sAttributeStrucs}:}{Object of class \code{"character"} the base node }
#' }
#' 
#' @section Methods:
#'   \describe{
#'    \item{show}{\code{signature(object = "partition")}: Display essential information }
#'    \item{sAttributes}{output of s-attributes in a partition}
#'    \item{addPos}{\code{signature(object="partition")}: add list with most frequent pos for a token }
#'    \item{[}{get frequency of a query}
#'    \item{[[}{shortcut to concordances for a query}
#'    }
#' 
#' @aliases partition-class show,partition-method [[,partition,ANY,ANY,ANY-method [,partition,ANY,ANY,ANY-method addPos,partition-method addPos [,partition-method [[,partition-method sAttributes,partition-method sAttributes
#' @rdname partition-class
#' @name partition-class
#' @exportClass partition
#' @docType class
#' @author Andreas Blaette
setClass("partition",
         representation(label="character", 
                        corpus="character",
                        encoding="character",
                        sAttributes="list",
                        explanation="character",
                        cpos="matrix",
                        pos="list",
                        size="numeric",
                        metadata="list",
                        strucs="numeric",
                        tf="list",
                        xml="character",
                        sAttributeStrucs="character"
         )
)


#' Initialize a partition
#' 
#' Set up an object of the partition class. Usually, this will include frequency lists.
#' 
#' The function sets up a partition based on a list of s-attributes with respective values.
#' The s-attributes defining the partition are a list, e.g. list(text_type="speech", text_year="2013").
#' The values of the list may contain regular expressions, but then the length always needs to be 1,
#' and the method needs to be set to "grep". Alternatively, the method can be set to "in", then
#' the length of the list may be > 1.
#' For the s-attributes, list elements may be character vectors with a length > 2.
#' However, the initialization of a partition object is considerably slower, if
#' you supply a >2 vector. The alternative being creating a set of partitions with
#' \code{partition.cluster} and then merging the partitions, this may still be ok.
#' For s-attributes, regular expressions can be used. Please note that for R grep,
#' double backlashes have to be used.
#' For some purposes (c.g. computation of crosstabulations), term frequencies will
#' not be needed in the setup of the partition object. In that case tf=FALSE and
#' metadata=FALSE will speed up the initialization of the object a lot.
#' A date range with a specific start and end date can be specified by providing dateRange.
#' Note that the sequence of the s-Attributes will matter. Things will speed up if you start 
#' with the conditions narrowing down the corpus the most.
#' 
#' @param corpus the CWB-corpus to be used
#' @param sAttributes list consisting of a set of character vectors (see
#' details)
#' @param label label of the new partition, defaults to "noLabel"
#' @param encoding encoding of the corpus (typically "LATIN1 or "(UTF-8)), if NULL, the encoding provided in the registry file of the corpus (charset="...") will be used b
#' @param tf either FALSE or TRUE, defaults to TRUE
#' @param metadata either FALSE or TRUE, defaults to TRUE
#' @param method either 'grep' or 'in' to specify the filtering method to get relevant strucs
#' @param xml either 'flat' (default) or 'nested'
#' @param verbose logical, defaults to TRUE
#' @return An object of the S4 class 'partition'
#' @author Andreas Blaette
#' @examples
#' \dontrun{
#' spd <- partition(list(text_party="SPD", text_type="speech"), "PLPRBTTXT"))
#' }
#' @import rcqp
#' @import methods
#' @importFrom chron seq.dates
#' @export partition
partition <- function(corpus, sAttributes, label=c(""), encoding=NULL, tf=TRUE, metadata=TRUE, method="grep", xml="flat", verbose=TRUE) {
  if (verbose==TRUE) message('Setting up partition ', label)
  Partition <- new('partition')
  Partition@corpus <- corpus
  Partition@label <- label
  Partition@sAttributes <- sAttributes
  Partition@sAttributeStrucs <- names(sAttributes)[length(sAttributes)]
  Partition@xml <- xml
  if(is.null(encoding)) {
    Partition@encoding <- .getCorpusEncoding(Partition@corpus)  
  } else {
    Partition@encoding <- encoding
  } 
  if (verbose==TRUE) message('... computing corpus positions and retrieving strucs')
  if (xml=="flat") {
    Partition <- .flatXmlSattributes2cpos(Partition, method)
  } else if (xml=="nested") {
    Partition <- .nestedXmlSattributes2cpos(Partition, method)
  } else {
    warning("WARNING: Value of 'xml' is not valid!")
  }
  if (verbose==TRUE) message('... computing partition size')
  Partition@size <- .partition.size(Partition)
  if (tf==TRUE) {
    if (verbose==TRUE) message('... computing term frequencies (for p-attribute word)')
    Partition@tf$word <- .cpos2tf(Partition, "word")
    if (verbose==TRUE) message('... computing term frequencies (for p-attribute lemma)')
    Partition@tf$lemma <- .cpos2tf(Partition, "lemma")      
  }
  if (metadata==TRUE) {
    if (verbose==TRUE) message('... setting up metadata (table and list of values)')
    Partition <- .partition.metadata(Partition, table=TRUE)
  }
  if (verbose==TRUE) message('... partition is set up\n')
  Partition
}

#' zoom into a partition
#' 
#' add a further specification of a s-attribute to an existing partition
#' @param Partition a partition object
#' @param sAttribute a list supplying further sAttributes
#' @param label a label for the new partition
#' @param method either "in" or "grep"
#' @param tf logical, whether to compute term frequencies
#' @export zoom
#' @rdname zoom
#' @name zoom
zoom <- function(Partition, sAttribute, label=c(""), method="in", tf=TRUE){
  newPartition <- new("partition")
  newPartition@corpus <- Partition@corpus
  message('Zooming into partition ', label)
  newPartition@label <- label  
  newPartition@sAttributes <- c(Partition@sAttributes, sAttribute)
  newPartition@sAttributeStrucs <- names(newPartition@sAttributes)[length(newPartition@sAttributes)]
  newPartition@xml <- Partition@xml
  newPartition@encoding <- Partition@encoding
  message('... specifying strucs and corpus positions')
  newPartition <- .zoomingSattributes2cpos(Partition, newPartition, sAttribute, method)
  message('... computing partition size')
  newPartition@size <- .partition.size(newPartition)
  if (tf==TRUE) {
    message('... computing term frequencies (for p-attribute word)')
    newPartition@tf$word <- .cpos2tf(newPartition, "word")
    message('... computing term frequencies (for p-attribute lemma)')
    newPartition@tf$lemma <- .cpos2tf(newPartition, "lemma")      
  }
  newPartition
}

#' Augment the partition object by strucs and cpos
#' 
#' @param Partition the partition object to be specified
#' @param newPartition the new partition
#' @param sAttribute info for specification (a list)
#' @param method either "in" or "grep"
#' @return an augmented partition object
#' @noRd
.zoomingSattributes2cpos <- function(Partition, newPartition, sAttribute, method){
  sAttr <- paste(Partition@corpus, '.', names(sAttribute), sep='')
  if (Partition@xml == "flat") {
    str <- cqi_struc2str(sAttr, Partition@strucs)    
  } else if (Partition@xml == "nested") {
    str <- cqi_struc2str(sAttr, cqi_cpos2struc(sAttr, Partition@cpos[,1]))    
  }
  if (method == "in") {
    hits <- which(str %in% sAttribute[[1]])
  } else if (method == "grep") {
    hits <- grep(sAttributes[[1]], str)
  }
  newPartition@cpos <- Partition@cpos[hits,]
  newPartition@strucs <- Partition@strucs[hits]
  if (length(Partition@metadata) == 2) {
    message('... adjusting metadata')
    newPartition@metadata$table <- Partition@metadata$table[hits,]
    newPartition <- .partitionMetadataValues(newPartition)
  }
  newPartition
}


#' generate the sattribute
#' 
#' Helper function for partition
#' 
#' @param corpus the CWB corpus used
#' @param dateRange a character with two character strings: the start date, and the end date
#' @return a character vector (length > 1) that can be used in sAttribute definition
#' @author Andreas Blaette
#' @export datesPeriod
datesPeriod <- function(corpus, dateRange) {
  sAttributeDate <- cqi_attributes(corpus, 's')[grep('date', cqi_attributes(corpus, 's'))]
  sAttr <- paste(corpus, '.', sAttributeDate, sep='')
  allDatesInCorpus <- unique(cqi_struc2str(sAttr, c(0:(cqi_attribute_size(sAttr)-1))))
  daysSequence <- strftime(seq.dates(from=strftime(dateRange[1], format="%m/%d/%Y"), to=strftime(dateRange[2], format="%m/%d/%Y"), by="days"), format="%Y-%m-%d")
  daysInCorpus <- allDatesInCorpus[which(allDatesInCorpus %in% daysSequence)]
  daysInCorpus
}


#' S3 partitionCluster class
#' 
#' it's simply a list with partition (class) objects
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{partition}:}{Object of class \code{"partition"} }
#'   }
#'   
#' @section Methods:
#'   \describe{
#'    \item{print}{get essential information about the partitionCluster}
#'    \item{partitionMerge}{(partitionCluster, label) merge the partitions in a partitionCluster into one partition}
#'    \item{as.DocumentTermMatrix}{(x, pAttribute)}
#'    \item{as.TermDocumentMatrix}{(x, pAttribute)}
#'    \item{as.matrix}{(x, pAttribute)}    
#'    }

#' @name partitionCluster-class
#' @aliases as.DocumentTermMatrix.partitionCluster as.matrix.partitionCluster as.TermDocumentMatrix.partitionCluster print.partitionCluster partitionMerge,partitionCluster-method partitionMerge.partitionCluster
#' @docType class
#' @rdname partitionCluster-class
NULL



#' Generate a list of partitions
#' 
#' A list of partition objects with fixed s-attributes and one variable
#' s-attribute is generated
#' 
#' If sAttributeVarValues is not given, all values for sAttributeVar in the partition
#' defined by sAttributesStatic will be retrived and used for defining the
#' partitions.
#' While generally S4 methods are used in the driller package, the return is a S3 method.
#' The reasons is that the number of partitions kept in the cluster is not known before the initialization.
#' Setting multicore to TRUE will speed up things. Error handling is less benevolent, risk of overheating, no verbose output.
#' 
#' @param corpus the CWB corpus to be used
#' @param sAttributesStatic a list with the definition of a partition that shall be prepared
#' @param sAttributeVar character vector indicating the s-attribute to be variabel
#' @param sAttributeVarValues character vector
#' @param encoding encoding of the corpus, if not provided, encoding provided in the registry file will be used
#' @param tf logical, whether term frequencies shall be generated
#' @param metadata logical, whether to set up metadata
#' @param method either 'grep' or 'in'
#' @return a S3 class 'partitionCluster', which is a list with partition objects
#' @importFrom parallel mclapply
#' @export partitionCluster
#' @aliases partitionCluster addPos,partitionCluster-method
#' @author Andreas Blaette
partitionCluster <- function(
  corpus,
  sAttributesStatic, sAttributeVar, sAttributeVarValues=c(),
  encoding=NULL, tf=TRUE, metadata=TRUE, method="grep"
  ) {
  multicore <- get("drillingControls", '.GlobalEnv')[['multicore']]
  multicoreMessage <- ifelse(
    multicore==TRUE,
    ' (use multicore: TRUE)',
    ' (use multicore: FALSE)'
    )
  message('\nPreparing cluster of partitions', multicoreMessage)
  cluster <- list()
  message('... setting up base partition')
  partitionBase <- partition(corpus, sAttributesStatic, tf=tf, metadata=metadata, method=method, verbose=FALSE)
  if (is.null(sAttributeVarValues)){
    message('... getting values of fixed s-attributes')
    sAttributeVarValues <- unique(cqi_struc2str(paste(corpus, '.', sAttributeVar, sep=''), partitionBase@strucs))
    message('... number of partitions to be initialized: ', length(sAttributeVarValues))
  }
  if (multicore==FALSE) {
    for (sAttribute in sAttributeVarValues){
      sAttr <- list()
      sAttr[[sAttributeVar]] <- sAttribute
      cluster[[sAttribute]] <- zoom(partitionBase, sAttribute=sAttr, label=sAttribute)
    }
  } else if (multicore==TRUE) {
    message('... setting up the partitions')
    cluster <- mclapply(
      sAttributeVarValues,
      function(x) zoom(
        partitionBase,
        sAttribute=sapply(sAttributeVar, function(y) x, USE.NAMES=TRUE),
        label=x
        )
      )
    names(cluster) <- sAttributeVarValues
  }
  class(cluster) <- "partitionCluster"
  cluster
}


#' add size of the partition to a partition object
#' 
#' The function requires the cpos in a partition object to be present. The size
#' of the partition is calculated based on the cpos. This is optimized / as good as it gets.
#' @noRd
.partition.size <- function(partition) sum(partition@cpos[,2]-partition@cpos[,1]+1)

#' @exportMethod show
#' @noRd
setMethod("show", "partition",
function(object){
  cat("** partition object **\n")
  cat(sprintf("%-20s", "CWB-corpus:"), object@corpus, "\n")
  cat(sprintf("%-20s", "Label:"), object@label, "\n")
  if (length(object@sAttributes)==0) {
    cat(sprintf("%-20s", "S-Attributes:"), "no specification\n")
  } else {
    s <- unlist(lapply(
      names(object@sAttributes),
      function(x) {paste(x, "=", paste(object@sAttributes[[x]], collapse="/"))}
      ))
    cat(sprintf("%-20s", "S-attributes:"), s[1], '\n')
    if (length(s)>1) {for (i in length(s)){cat(sprintf("%-20s", " "), s[i], '\n')}}
  } 
  cat(sprintf("%-21s", "Corpus positions:"))
  if (nrow(object@cpos)==0) {cat("not available\n")}
  else {cat(nrow(object@cpos), "pairs of corpus positions\n")}
  cat(sprintf("%-21s", "Partition size:"))
  if (is.null(object@size)) {cat("not available\n")}
  else {cat(object@size, "tokens\n")}
  cat(sprintf("%-21s", "Term frequencies:"))
  if (length(object@tf)==0) {cat("not available\n")}
  else {cat("available\n")}
})

#' Print method for partitionCluster Objects 
#'
#' Prints the number of partitions in the cluster and returns the respective sizes
#' 
#' @param object the partitionCluster object
#' @method print partitionCluster
#' @noRd
print.partitionCluster <- function (object) {
            cat("\n** PartitionCluster object: **\n")
            cat('Cluster includes', length(object), 'partition objects:\n')
            for (i in c(1: length(object))) {
              cat(names(object)[i], '(', object[[i]]@size, ' token)\n', sep='')
            }
}


#' add metadata information to a partition object
#' 
#' This function is usually part of the procedure to set up a partition. Based
#' on the corpus positions that have initially been set up, a table with all
#' metadata is added to the object, and a list with the attributes and the
#' values that occur.
#' 
#' The values of the metadata are there to determine which subcorpora can be
#' generated. The table with all metadata can be added as it is a basis for the
#' values. It can be omitted for performance and memora reasons.
#' 
#' @param partition a partition object
#' @param table logical, defaults to FALSE
#' @return A 'metadata' item will be added to the partition object, with the
#' two following subitems:
#' table - the table described above
#' values - a list of character vectors
#' @author Andreas Blaette
#' @noRd
.partition.metadata <- function(Partition, table=FALSE) {
  m <- cqi_attributes(Partition@corpus, 's')
  if (table==TRUE) {
    if (Partition@xml == "flat") {
      Partition@metadata$table <- data.frame(sapply(m, USE.NAMES=TRUE, function(x) cqi_struc2str(paste(Partition@corpus, '.', x, sep=''), Partition@strucs)))
    } else if (Partition@xml == "nested") {
      meta <- vapply(m, FUN.VALUE="character", USE.NAMES=TRUE, function(x)paste(Partition@corpus, '.', x, sep=''))
      Partition@metadata$table <- data.frame(sapply(meta, USE.NAMES=TRUE,
                                                    function(x) cqi_struc2str(x, cqi_cpos2struc(x, Partition@cpos[,1]))
                                                    ))
      colnames(Partition@metadata$table) <- names(meta)
    }
  }
  Partition <- .partitionMetadataValues(Partition)
  return(Partition)
}

#' Helper function
#' @noRd
.partitionMetadataValues <- function(Partition){
  Partition@metadata$values <- sapply(
    sAttributes(Partition),
    USE.NAMES=TRUE,
    function(x){
      foo<-unique(cqi_struc2str(paste(Partition@corpus, '.', x, sep=''), Partition@strucs));
      Encoding(foo)<-Partition@encoding;
      foo}
    )
  Partition
}

#' Get the cpos for a partition based on sattributes
#' 
#' Augment the partition object
#' 
#' The function works nicely - potentially, it can be optimized, but I have tried many things.
#' Interestingly, the for-loop is more effective than a vectorized version
#' @param partition the rudimentary partition object
#' @return an augmented partition object (includes now cpos and strucs)
#' @noRd
.flatXmlSattributes2cpos <- function(part, method){
  root <- paste(part@corpus, '.', part@sAttributeStrucs, sep='')
  meta <- data.frame(struc=c(0:(cqi_attribute_size(root)-1)), select=rep(0, times=cqi_attribute_size(root)))
  if (length(part@sAttributes) > 0) {
    for (s in names(part@sAttributes)){
      sattr <- paste(part@corpus, ".", s, sep="")
      meta[,2] <- as.vector(cqi_struc2str(sattr, meta[,1]))
      Encoding(meta[,2]) <- part@encoding
      if (method=="in") {
        meta <- meta[which(meta[,2] %in% part@sAttributes[[s]]),]
      } else {
        meta <- meta[grep(part@sAttributes[[s]], meta[,2]),]
      }
    }
  }
  part@cpos <- matrix(data=unlist(lapply(meta[,1], function(x)cqi_struc2cpos(root, x))),
         ncol=2,
         byrow=TRUE
         )
  part@strucs <- as.numeric(meta[,1])
  part
}

#' Get the cpos for a partition based on sattributes
#' 
#' Augment the partition object by strucs and cpos
#' 
#' @param partition the rudimentary partition object
#' @return an augmented partition object (includes now cpos and strucs)
#' @noRd
.nestedXmlSattributes2cpos <- function(Partition, method){
  sAttr <- vapply(
    names(Partition@sAttributes),
    USE.NAMES=TRUE, FUN.VALUE="character",
    function(x)paste(Partition@corpus, '.', x, sep='')
    )
  sAttr <- rev(sAttr)
  strucs <- c(0:(cqi_attribute_size(sAttr[1])-1))
  cpos <- matrix(
    unlist(lapply(strucs,
                  function(x) cqi_struc2cpos(sAttr[1], x)
                  )
           ), byrow=TRUE, ncol=2
  )
  for (i in c(1:length(sAttr))){
    if ( i == 1) {
      meta <- cqi_struc2str(sAttr[i], strucs)
    } else if ( i > 1 ) {
      meta <- cqi_struc2str(sAttr[i], cqi_cpos2struc(sAttr[i], cpos[,1]))
    }
    # Encoding(meta) <- Partition@encoding
    if (method == "in") {
      hits <- which(meta %in% Partition@sAttributes[[names(sAttr)[i]]])
    } else if (method == "grep") {
      hits <- grep(Partition@sAttributes[[names(sAttr)[i]]], meta)
    }
    cpos <- cpos[hits,]
    strucs <- strucs[hits]
  }
  Partition@strucs <- strucs
  Partition@cpos <- cpos
  Partition
}

#' add POS attributes
#'
#' @param object the object to be passed
#' @param ... further parameters
#' @author Andreas Blaette
#' @export
#' @docType methods
#' @rdname addPos-methods
#' @noRd
setGeneric("addPos", function(object,...){standardGeneric("addPos")})

#' Fill slot 'pos' of a partition object with tables giving the statistic of pos
#' 
#' Augment the partition object by strucs and cpos
#' 
#' @param object a partition class object
#' @param pos character vector - pos statistic for lemma or word
#' @return an augmented partition object (includes pos now)
#' @author Andreas Blaette
#' @docType methods
#' @exportMethod addPos
#' @noRd
setMethod("addPos", "partition",
          function(object, pos){
  cpos <- unlist(apply(object@cpos, 1, function(x) c(x[1]:x[2])))
  bag <- data.frame(
    word=cqi_cpos2id(paste(object@corpus, '.word', sep=''), cpos),
    lemma=cqi_cpos2id(paste(object@corpus, '.lemma', sep=''), cpos),
    pos=cqi_cpos2id(paste(object@corpus, '.pos', sep=''), cpos),
    tf <- rep(1, length(cpos))
  )
  for (pAttr in pos) {
    object@pos[[pAttr]] <- list()
    crosstab <- table(bag[,c(pAttr, "pos")])
    rownames(crosstab) <- cqi_id2str(paste(object@corpus, '.', pAttr, sep=''), as.integer(rownames(crosstab)))
    colnames(crosstab) <- cqi_id2str(paste(object@corpus, '.pos', sep=''), as.integer(colnames(crosstab)))
    object@pos[[pAttr]][["abs"]] <- crosstab
    Encoding(rownames(object@pos[[pAttr]][["abs"]])) <- object@encoding
    object@pos[[pAttr]][["rel"]] <- t(apply(crosstab, 1, function(x) round(x/sum(x)*100, 2)))
    Encoding(rownames(object@pos[[pAttr]][["rel"]])) <- object@encoding
    object@pos[[pAttr]][["max"]] <- apply(crosstab, 1, function(x) colnames(crosstab)[which.max(x)])
    Encoding(names(object@pos[[pAttr]][["max"]])) <- object@encoding
  }
  object
})

#' perform addPos and further adjustments for all partitions in a cluster
#' 
#' @param cluster a partitionCluster object
#' @param pos character vector: "word", "lemma", or both
#' @param posFilter pos to keep
#' @param minFrequency minimum frequency of tokens
#' @export adjust
#' @rdname adjust
#' @name adjust
adjust <- function(cluster, pos=c(""), posFilter=c(), minFrequency=0){
  pimping <- function(p) {
    new <- addPos(p, pos)
    if (!is.null(posFilter)) {
      for (i in pos){
        new@tf[[i]] <- new@tf[[i]][which(new@pos[[i]]$max %in% posFilter),]
      }
    }
    if (minFrequency > 0){
      for (i in pos){
        new@tf[[i]] <- new@tf[[i]][which(new@tf[[i]][,"tf"]>=minFrequency),]
      }
    }
    new 
  }
  if (get('drillingControls', '.GlobalEnv')[['multicore']] == TRUE) {
    pimpedCluster <- mclapply(cluster, FUN = pimping)
  } else {
    pimpedCluster <- lapply(cluster, FUN=pimping)    
  }
  class(pimpedCluster) <- "partitionCluster"
}

setGeneric("partitionMerge", function(object,...){standardGeneric("partitionMerge")})

#' Merge the partitions in a cluster into one partition
#' 
#' The partitions in a cluster object will be merged into one new partition
#' 
#' The function aggregates several partitions into one partition. The
#' prerequisite for this function to work properly is that there are no
#' overlaps of the different partitions that are to be summarized.
#' Encodings and the root node need to be identical, too.
#' 
#' @param cluster a cluster object
#' @param label the label for the new partition
#' @return An object of the class 'partition. See partition for the
#' details on the class.
#' @author Andreas Blaette
#' @S3method partitionMerge partitionCluster
#' @noRd
partitionMerge.partitionCluster <- function(cluster, label){
  cat('There are', length(cluster), 'partitions to be merged\n')
  corpora <- unique(unlist(lapply(names(cluster), function(x)cluster[[x]]@corpus)))
  if (!all(corpora==cluster[[1]]@corpus)) print("WARNING: This function will not work correctly, as the cluster comprises different corpora")
  message('... merging the struc vectors')
  strucs <- c()
  for (name in names(cluster)) {strucs <- union(strucs, cluster[[name]]@strucs)}
  message('... generating corpus positions')
  cpos <- data.matrix(t(data.frame(lapply(strucs, function(x){cqi_struc2cpos(paste(corpora,'.', 'text', sep=''),x)}))))
  rownames(cpos) <- NULL
  partition <- new("partition")
  partition@corpus <- corpora
  partition@strucs <- strucs
  partition@cpos <- cpos
  partition@encoding <- unique(unlist(lapply(names(cluster), function(x)cluster[[x]]@encoding)))
  partition@sAttributeStrucs <- unique(unlist(lapply(names(cluster), function(x)cluster[[x]]@sAttributeStrucs)))
  partition@explanation=c(paste("this partition is a merger of the partitions", paste(names(cluster), collapse=', ')))
  cat('... computing corpus size\n')
  partition@size <- .partition.size(partition)
  cat('... computing term frequencies (for p-attribute word)\n')
  tfmatrix <- .cluster2tfmatrix(cluster, 'word')
  partition@tf$word <- data.frame(
    row.names=rownames(tfmatrix$abs),
    id=cqi_str2id(paste(corpora,'.', 'word',sep=''), rownames(tfmatrix$abs)),
    wc=rowSums(tfmatrix$abs)
  ) 
  cat('... computing term frequencies (for p-attribute lemma)\n')
  tfmatrix <- .cluster2tfmatrix(cluster, 'lemma')
  partition@tf$lemma <- data.frame(
    row.names=rownames(tfmatrix$abs),
    id=cqi_str2id(paste(corpora,'.', 'word',sep=''), rownames(tfmatrix$abs)),
    wc=rowSums(tfmatrix$abs)
  )                      
  cat('... setting up metadata (table and list of values)\n')
  partition <- .partition.metadata(partition, table=TRUE)
  partition@label <- label
  partition
}

#' @exportMethod [[
setMethod('[[', 'partition', function(x,i){
  context <- context(
    i, x,
    get('drillingControls', '.GlobalEnv')[['pAttribute']],
    get('drillingControls', '.GlobalEnv')[['leftContext']],
    get('drillingControls', '.GlobalEnv')[['rightContext']],
    get('drillingControls', '.GlobalEnv')[['minSignificance']],
    get('drillingControls', '.GlobalEnv')[['posFilter']],
    verbose=FALSE
    )
  context
}
)

#' @exportMethod [
setMethod('[', 'partition', function(x,i){
  hits <- nrow(.queryCpos(i,x))
  hits
}
)

#' merge either two partitionx matrices, or a tf matrix and a partition
#' matrix
#' 
#' The function either takes two partition objects with tf lists as an input,
#' or a tf matrix and a tf list. It will then expand the first list by the
#' second list. The function is used by cluster2tfmatrix.
#' 
#' @param x either a partition object with tf frequencies or a tf-matrix
#' @param y a partition object with tf frequencies
#' @param pattribute a p-attribute
#' @return you get a tf matrix with the tokens in the rows and the subcorpora
#' in the columns
#' @author Andreas Blaette
#' @noRd
.partition2tfmatrix <- function(x, y, pattribute){
  b <- cbind(rownames(y@tf[[pattribute]]), y@tf[[pattribute]][,2])    
  if (class(x)=='partition'){
    partition.labels <- x@label
    a <- cbind(rownames(x@tf[[pattribute]]), x@tf[[pattribute]][,2])
  }
  else {
    partition.labels <- colnames(x)
    a <- cbind(rownames(x), x)
  }
  m <- merge(a,b, by.x=1, by.y=1, all=TRUE)
  rownames(m) <- m[,1]
  m <- m[,2:ncol(m)]
  colnames(m) <- c(partition.labels, y@label)  
  m
}




#' prepare a tf-matrix based on a partition cluster that includes frequency
#' counts
#' 
#' The function merges the frequency counts (for p-attribute 'word' or 'token')
#' that are contained in the partition objects included in a partition object
#' into a tf matrix.
#' 
#' The function merges the given frequency lists using strings. The function
#' may be potentially faster, it uses ids, which has not been implemented so
#' far.
#' 
#' @param cluster cluster of partition objects
#' @param pattribute typically 'word' or 'token'
#' @return A list with the following items:
#' abs - a matrix with the absolute frequencies
#' rel - a matrix with the relative frequencies
#' sizes - a vector with the sizes of the subcorpora
#' @author Andreas Blaette
#' @noRd
.cluster2tfmatrix <- function(cluster, pattribute){
  cat(.takeoff(), '... there are',length(cluster),'clusters to merge\n')
  cat(.takeoff(), '... merging frequency lists 1 and 2 /', cluster[[1]]@label,'and', cluster[[2]]@label,'\n')
  tf <- list()
  tf$abs <- .partition2tfmatrix(cluster[[1]], cluster[[2]], pattribute)
  if (length(cluster)>2){
    for (i in 3:length(cluster)){
      cat(.takeoff(), '... proceeding to merger', i, '- adding', cluster[[i]]@label,'\n')
      tf$abs <- .partition2tfmatrix(tf$abs, cluster[[i]], pattribute)
    }
  }
  tf$abs <- matrix(as.numeric(as.vector(unlist(tf$abs))), nrow=nrow(tf$abs), dimnames=dimnames(tf$abs))
  tf$abs[is.na(tf$abs)] <- 0
  tf$sizes <- sapply(names(cluster), function(x)sum(cluster[[x]]@tf[[pattribute]][,2]))
  tf$rel <- tf$abs/tf$sizes
  tf
}


setGeneric("sAttributes", function(object,...){standardGeneric("sAttributes")})

#' Print S-Attributes in a partition
#' 
#' Convencience function - just to access the s-sttributes in a partition
#' quickly.
#'
#' @param partition a partition object
#' @return the S-Attributes are immediately printed
#' @exportMethod sAttributes
#' @noRd
setMethod(
  "sAttributes", "partition",
  function (object) {
    sAttributes <- cqi_attributes(object@corpus, "s")
    sAttributes
  }
)

.getCorpusEncoding <- function(corpus){
  registry <- scan(
    file=file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(corpus)),
    sep="\n",
    what="character",
    quiet=TRUE
  )
  encodingLine <- registry[grep('charset\\s*=\\s*"', registry)]
  encoding <- sub('^.*charset\\s*=\\s*"(.+?)".*$', "\\1", encodingLine)
  encoding <- toupper(encoding)
  if (!encoding %in% iconvlist()){
    warning('Please check encoding in the registry file (charset="..." provides unknown encoding) or provide encoding explicitly')
  }
  return(tolower(encoding))
}