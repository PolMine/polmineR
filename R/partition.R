#' partition class
#' 
#' Basic class for for almost anything in the driller package.
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
#'    \item{addPos}{\code{signature(object="partition")}: add list with most frequent pos for a token }
#'    \item{trim}{\code{signature(object="partition")}: trim a partition object }
#'    \item{tf}{\code{signature(object="partition")}: get term frequencies }
#'    \item{as.partitionCluster}{\code{signature(object="partition")}: transform a partition object into a partitionCluster (to add further objects) }
#'    \item{[}{get frequency of a query}
#'    \item{html}{transform partition to html}
#'    }
#' 
#' @aliases partition-class show,partition-method [,partition,ANY,ANY,ANY-method 
#'   [,partition-method as.partitionCluster
#'   as.partitionCluster,partition-method export export,partition-method split
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
#' @param def list consisting of a set of character vectors (see
#' details)
#' @param label label of the new partition, defaults to "noLabel"
#' @param encoding encoding of the corpus (typically "LATIN1 or "(UTF-8)), if NULL, the encoding provided in the registry file of the corpus (charset="...") will be used b
#' @param tf the pAttributes for which term frequencies shall be retrieved
#' @param meta a character vector
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
partition <- function(
  corpus,
  def,
  label=c(""),
  encoding=NULL,
  tf=c("word", "lemma"),
  meta=NULL,
  method="grep",
  xml="flat",
  verbose=TRUE
  ) {
  if (!corpus %in% cqi_list_corpora()) warning("corpus is not an available CWB corpus")
  if (verbose==TRUE) message('Setting up partition ', label)
  Partition <- new('partition')
  if ((corpus %in% cqi_list_corpora()) == FALSE) warning("corpus not in registry - maybe a typo?")
  Partition@corpus <- corpus
  if(is.null(encoding)) {
    Partition@encoding <- .getCorpusEncoding(Partition@corpus)  
  } else {
    Partition@encoding <- encoding
  }
  if (verbose==TRUE) message('... encoding of the corpus is ', Partition@encoding)
  Partition@label <- label
  Partition@sAttributes <- lapply(def, function(x).adjustEncoding(x, Partition@encoding))  
  Partition@sAttributeStrucs <- names(def)[length(def)]
  Partition@xml <- xml
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
  if (!is.null(tf)) {if (tf[1] == FALSE) {tf <- NULL}}
  if (length(tf>0)) {
    for (p in tf){
      if (verbose==TRUE) message('... computing term frequencies (for p-attribute ', p, ')')  
      Partition@tf[[p]] <- .cpos2tf(Partition, p)
    }
  }
  if (!is.null(meta)) {
    if (verbose==TRUE) message('... setting up metadata (table and list of values)')
    Partition <- .partition.metadata(Partition, meta)
  }
  if (verbose==TRUE) message('... partition is set up\n')
  Partition
}

#' zoom into a partition
#' 
#' add a further specification of a s-attribute to an existing partition
#' 
#' @param Partition a partition object
#' @param def a list supplying further sAttributes
#' @param label a label for the new partition
#' @param method either "in" or "grep"
#' @param tf character vector, pAttributes for which term frequencies shall be retrieved
#' @param verbose logical, show progress report or not (defaults to TRUE)
#' @export zoom
#' @rdname zoom
#' @name zoom
zoom <- function(Partition, def, label=c(""), method="in", tf=c("word", "lemma"), verbose=TRUE){
  newPartition <- new("partition")
  newPartition@corpus <- Partition@corpus
  message('Zooming into partition ', label)
  newPartition@label <- label  
  def <- lapply(def, function(x).adjustEncoding(x, Partition@encoding))  
  newPartition@sAttributes <- c(Partition@sAttributes, def)
  newPartition@sAttributeStrucs <- names(newPartition@sAttributes)[length(newPartition@sAttributes)]
  newPartition@xml <- Partition@xml
  newPartition@encoding <- Partition@encoding
  message('... specifying strucs and corpus positions')
  newPartition <- .zoomingSattributes2cpos(Partition, newPartition, def, method)
  message('... computing partition size')
  newPartition@size <- .partition.size(newPartition)
  if (length(tf)>0) {
    for (p in tf){
      if (verbose==TRUE) message('... computing term frequencies (for p-attribute ', p, ')')  
      newPartition@tf[[p]] <- .cpos2tf(newPartition, p)
    }
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
  Encoding(str) <- newPartition@encoding
  if (method == "in") {
    hits <- which(str %in% sAttribute[[1]])
  } else if (method == "grep") {
    hits <- grep(sAttributes[[1]], str)
  }
  newCpos <- Partition@cpos[hits,]
  if (class(newCpos) == "matrix"){
    newPartition@cpos <- newCpos
  } else if (class(newCpos) == "integer") {
    newPartition@cpos <- matrix(newCpos, ncol=2, byrow=TRUE)     
  }
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
  else {cat("available for", paste(names(object@tf), collapse=", "), "\n")}
})


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
#' @param meta a character vector
#' @param table whether to setup metadata table
#' @return A 'metadata' item will be added to the partition object, with the
#' two following subitems:
#' table - the table described above
#' values - a list of character vectors
#' @author Andreas Blaette
#' @noRd
.partition.metadata <- function(Partition, meta, table=TRUE) {
  m <- meta
  meta <- NULL
  if (table==TRUE) {
    if (Partition@xml == "flat") {
      Partition@metadata$table <- data.frame(
        sapply(
          m,
          USE.NAMES=TRUE,
          function(x) { 
            tmp <- cqi_struc2str(paste(Partition@corpus, '.', x, sep=''), Partition@strucs)
            Encoding(tmp) <- Partition@encoding
            tmp
          }
          )
        )
    } else if (Partition@xml == "nested") {
      meta <- vapply(m, FUN.VALUE="character", USE.NAMES=TRUE, function(x)paste(Partition@corpus, '.', x, sep=''))
      Partition@metadata$table <- data.frame(
        sapply(
          meta,
          USE.NAMES=TRUE,
          function(x) {
            tmp <- cqi_struc2str(x, cqi_cpos2struc(x, Partition@cpos[,1]))
            Encoding(tmp) <- Partition@encoding
            tmp
          }
          )
        )
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
    Encoding(meta) <- Partition@encoding
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


#' @exportMethod [
setMethod('[', 'partition', function(x,i){
  hits <- nrow(.queryCpos(i,x))
  hits
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





#' split partition into partitionCluster
#' 
#' Split a partition object into a partition Cluster if gap between strucs
#' exceeds a minimum number of tokens specified by 'gap'. Relevant to 
#' split up a plenary protocol into speeches. Note: To speed things up, the
#' returned partitions will not include frequency lists. The lists can be
#' prepared by applying \code{enrich} on the partitionCluster object that
#' is returned.
#' 
#' @param x a partition object
#' @param gap an integer specifying the minimum gap for performing the split
#' @param drop not yet implemented
#' @return a partitionCluster
#' @aliases split,partition
#' @rdname split-partition-method 
#' @exportMethod split
setMethod("split", "partition", function(x, gap, drop=FALSE, ...){
  # if (length(x@metadata) == 0) warning("no metadata, method potentially fails -> please check what happens")
  cpos <- x@cpos
  if (nrow(cpos) > 1){
    distance <- cpos[,1][2:nrow(cpos)] - cpos[,2][1:(nrow(cpos)-1)]
    beginning <- c(1, ifelse(distance>gap, 1, 0))
    no <- vapply(1:length(beginning), FUN.VALUE=1, function(x) ifelse (beginning[x]==1, sum(beginning[1:x]), 0))
    for (i in (1:length(no))) no[i] <- ifelse (no[i]==0, no[i-1], no[i])
    strucsClassified <- cbind(x@strucs, no)
    strucList <- split(strucsClassified[,1], strucsClassified[,2])
    cposClassified <- cbind(cpos, no)
    cposList1 <- split(cposClassified[,1], cposClassified[,3])
    cposList2 <- split(cposClassified[,2], cposClassified[,3])
    clusterRaw <- lapply(c(1:length(strucList)), function(i) {
      p <- new("partition")
      p@strucs <- strucList[[i]]
      p@cpos <- cbind(cposList1[[i]], cposList2[[i]])
      p@corpus <- x@corpus
      p@encoding <- x@encoding
      p@sAttributes <- x@sAttributes
      p@explanation <- c("partition results from split, sAttributes do not necessarily define partition")
      p@xml <- x@xml
      p@sAttributeStrucs <- x@sAttributeStrucs
      p@label <- paste(x@label, i, collapse="_", sep="")
      if (is.null(names(x@metadata))){
        meta <- NULL
      } else {
        meta <- colnames(x@metadata$table)
      }
      p <- enrich(
        p, size=TRUE,
        tf=NULL,
        meta=meta,
        verbose=TRUE
      )
      p
    })
  } else {
    clusterRaw <- list(x)
  }
  names(clusterRaw) <- unlist(lapply(clusterRaw, function(y) y@label))
  cluster <- as.partitionCluster(clusterRaw)
  cluster
})


