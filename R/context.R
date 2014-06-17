#' class for kwic output
#' 
#' information for kwic output
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{cpos}:}{Object of class \code{"list"} corpus positions }
#'     \item{\code{word}:}{Object of class \code{"list"} to be explained }
#'  }
#' @name kwic-class
#' @rdname kwic-class
#' @docType class
#' @exportClass kwic
setClass("kwic",
         representation(cpos="list",
                        word="list"
         )
)

.filter <- list(
  include=function(x,y) {x %in% y},
  exclude=function(x,y) {!(x %in% y)}
  )

#' S4 context class
#' 
#' class to organize information of context analysis
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{query}:}{Object of class \code{"character"} node examined }
#'     \item{\code{frequency}:}{Object of class \code{"numeric"} number of hits }
#'     \item{\code{partition}:}{Object of class \code{"character"} the partition the analysis is based on }
#'     \item{\code{partitionSize}:}{Object of class \code{"numeric"} the size of the partition }
#'     \item{\code{kwic}:}{Object of class \code{"kwic"} ~~ }
#'     \item{\code{left.context}:}{Object of class \code{"numeric"} number of tokens to the right }
#'     \item{\code{right.context}:}{Object of class \code{"numeric"} number of tokens to the left }
#'     \item{\code{size}:}{Object of class \code{"numeric"} number of tokens in the right and left context }
#'     \item{\code{pattribute}:}{Object of class \code{"character"} p-attribute of the query }
#'     \item{\code{corpus}:}{Object of class \code{"character"} the CWB corpus used }
#'     \item{\code{stat}:}{Object of class \code{"data.frame"} statistics of the analysis }
#'     \item{\code{encoding}:}{Object of class \code{"character"} encoding of the corpus }
#'     \item{\code{posFilter}:}{Object of class \code{"character"} part-of-speech tags filtered}
#'     \item{\code{cpos}:}{Object of class \code{"list"} corpus positions of the hits }
#'     \item{\code{statisticalTest}:}{Object of class \code{"character"} statistical test used }
#'     \item{\code{statSummary}:}{Object of class \code{"data.frame"} statistical summary }
#'   }
#' @section Methods:
#'   \describe{
#'     \item{show}{output of core information}
#'     \item{summary}{core statistical information}
#'     \item{[}{index the object}
#'     \item{[[}{specific collocates}
#'     \item{trim}{trim the object}
#'    }
#'     
#' @name context-class
#' @aliases show,context-method [,context-method [,context,ANY,ANY,ANY-method [[,context-method summary,context-method trim,context-method
#' @docType class
#' @exportClass kwic
#' @rdname context-class
setClass("context",
         representation(query="character",
                        frequency="numeric",
                        partition="character",
                        partitionSize="numeric",
                        kwic="kwic",
                        left.context="numeric",
                        right.context="numeric",
                        size="numeric",
                        pattribute="character",
                        corpus="character",
                        stat="data.frame",
                        encoding="character",
                        posFilter="character",
                        cpos="list",
                        statisticalTest="character",
                        statisticalSummary="data.frame"
                        )
)



#' concordances (S4 class)
#' 
#' S4 class for organizing information for concordance output
#' 
#' @section Slots:
#'   \describe{
#'    \item{\code{metadata}:}{Object of class \code{"character"} keeping the sAttributes of the metadata that are to be displayed }
#'    \item{\code{table}:}{Object of class \code{"data.frame"} a table with the relevant information for kwic output }
#'    \item{\code{collocate}:}{Object of class \code{"character"} collocate, if applicable }
#'    \item{\code{encoding}:}{Object of class \code{"character"} encoding of the corpus }
#'   }
#' @section Methods:
#'   \describe{
#'    \item{[}{indexing for seeing only some concordances}
#'    \item{show}{get kwic output}
#'   }
#'   
#' @name concordances-class
#' @docType class
#' @aliases show,concordances-method concordances-class [,concordances,ANY,ANY,ANY-method [,concordances-method
#' @exportClass concordances
#' @rdname concordances-class
setClass("concordances",
         representation(metadata="character",
                        collocate="character",
                        table="data.frame",
                        encoding="character"
         )
)


#' trim and filter a context object
#' 
#' Trim 
#' 
#' Maybe it would be more efficient to use the subset function.-
#' 
#' @param object a context object to be filtered
#' @param minSignificance minimum significance level
#' @param minFrequency the minimum frequency
#' @param maxRank maximum rank
#' @param posFilter exclude words with a POS tag not in this list
#' @param tokenFilter tokens to exclude from table
#' @author Andreas Blaette
#' @include generics.R
#' @exportMethod trim
#' @docType methods
#' @noRd
setMethod("trim", signature(object="context"), function(object, minSignificance=0, minFrequency=0, maxRank=0, posFilter=NULL, tokenFilter=NULL){
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


setGeneric("context", function(object, ...){standardGeneric("context")})


#' Analyze context of a node word
#' 
#' Retrieve the concordances of a token and calculate the log-likelihood test for collocates
#' 
#' For formulating the query, CPQ syntax may be used (see examples).
#' 
#' @param object a partition or a partitionCluster object
#' @param query query, which may by a character vector or a cqpQuery object
#' @param pAttribute p-attribute of the query
#' @param leftContext no of tokens and to the left of the node word
#' @param rightContext no of tokens to the right of the node word
#' @param minSignificance minimum log-likelihood value
#' @param posFilter character vector with the POS tags to be included - may not be empty!!
#' @param filterType either "include" or "exclude"
#' @param stopwords exclude a query hit from analysis if stopword(s) is/are in context
#' @param statisticalTest either "LL" (default) or "pmi"
#' @param verbose report progress, defaults to TRUE
#' @return depending on whether a partition or a partitionCluster serves as input, the return will be a context object, or a contextCluster object
#' @author Andreas Blaette
#' @include generics.R partition.R
#' @aliases context context,partition-method context,partitionCluster-method as.matrix,contextCluster-method as.TermContextMatrix,contextCluster-method context,contextCluster-method
#' @examples
#' \dontrun{
#' p <- partition(list(text_type="speech"), "PLPRBTTXT")
#' a <- context('"Integration"', p)
#' }
#' @importFrom parallel mclapply
#' @rdname context-method
#' @docType methods
#' @name context
#' @exportMethod context
setMethod("context", "partition",
  function(
    object,
    query,
    pAttribute="useControls",
    leftContext=0,
    rightContext=0,
    minSignificance=0,
    posFilter=c(),
    filterType="useControls",
    stopwords=c(),
    statisticalTest="LL",
    verbose=TRUE
  ) {
  if (pAttribute == "useControls") pAttribute <- get("drillingControls", '.GlobalEnv')[['pAttribute']]
  if (leftContext == 0) leftContext <- get("drillingControls", '.GlobalEnv')[['leftContext']]
  if (rightContext == 0) rightContext <- get("drillingControls", '.GlobalEnv')[['rightContext']]
  if (minSignificance == -1) minSignificance <- get("drillingControls", '.GlobalEnv')[['minSignificance']]
  if (is.null(posFilter)) posFilter <- get("drillingControls", '.GlobalEnv')[['posFilter']]
  if (filterType == "useControls") filterType <- get("drillingControls", '.GlobalEnv')[['filterType']]
  multicore <- get("drillingControls", '.GlobalEnv')[['multicore']]
  ctxt <- new("context")
  ctxt@query <- query
  ctxt@pattribute <- pAttribute
  ctxt@corpus <- partition@corpus
  ctxt@left.context <- leftContext
  ctxt@right.context <- rightContext
  ctxt@encoding <- partition@encoding
  ctxt@posFilter <- posFilter
  ctxt@partition <- partition@label
  ctxt@partitionSize <- partition@size
  ctxt@statisticalTest <- statisticalTest
  if (verbose==TRUE) message('Analysing the context for node word "', query,'"')
  corpus.pattr <- paste(ctxt@corpus,".", pAttribute, sep="")
  corpus.sattr <- paste(ctxt@corpus,".text_id", sep="")
  if (verbose==TRUE) message("... getting counts for query in partition", appendLF=FALSE)
  # query <- .adjustEncoding(query, partition@encoding)
  # Encoding(query) <- ctxt@encoding
  hits <- .queryCpos(ctxt@query, partition, pAttribute)
  hits <- cbind(hits, cqi_cpos2struc(corpus.sattr, hits[,1]))
  hits <- apply(hits, 1, function(x) as.list(unname(x)))
  message(' (', length(hits), " occurrences)")
  concordances <- new("kwic")
  stopwordId <- unlist(lapply(stopwords, function(x) cqi_regex2id(corpus.pattr, x)))
  if (verbose==TRUE) message("... counting tokens in context ")
  if (multicore==TRUE) {
    bigBag <- mclapply(hits, function(x) .surrounding(x, ctxt, corpus.sattr, filterType, stopwordId))
  } else {
    bigBag <- lapply(hits, function(x) .surrounding(x, ctxt, corpus.sattr, filterType, stopwordId))
  }
  bigBag <- bigBag[!sapply(bigBag, is.null)]
  if (!is.null(stopwordId)){
    message("... hits filtered because stopword(s) occur in context: ", (length(hits)-length(bigBag)))
  }
  ctxt@cpos <- lapply(bigBag, function(x) x$cpos)
  wc <- table(unlist(lapply(bigBag, function(x) x$id)))
  ctxt@size <- length(unlist(lapply(bigBag, function(x) unname(unlist(x$cpos)))))
  ctxt@frequency <- length(bigBag)
  if (verbose==TRUE) message('... context size: ', ctxt@size)
  if (statisticalTest == "LL"){
    if (verbose==TRUE) message("... performing log likelihood test")
    calc <- .g2Statistic(as.integer(names(wc)), unname(wc), ctxt@size, partition, pAttribute)
    ctxt@statisticalSummary <- .statisticalSummary(ctxt)
  } else if (statisticalTest=="pmi"){
    if (verbose==TRUE) message("... calculating pointwise mutual information")
    calc <- .pmi(
      windowIds=as.integer(names(wc)),
      windowFreq=unname(wc),
      countTarget=ctxt@frequency,
      partitionObject=partition,
      pAttribute=pAttribute
      )
  } else {
    warning("test suggested not supported")
  }
  ctxt@stat <- as.data.frame(cbind(
    rank=1:nrow(calc),
    calc
  ))
  ctxt <- trim(ctxt, minSignificance=minSignificance)
  rownames(ctxt@stat) <- cqi_id2str(corpus.pattr, ctxt@stat[,"collocateId"])
  Encoding(rownames(ctxt@stat)) <- partition@encoding
  ctxt
})

.surrounding <- function (set, ctxt, corpus.sattr, filterType, stopwordId) {
  bag <- list()
  set <- as.numeric(set)
  cpos.left <- c((set[1]-ctxt@left.context):(set[1]-1))
  cpos.left <- cpos.left[which(cqi_cpos2struc(corpus.sattr, cpos.left)==set[3])]
  cpos.right <- c((set[2]+1):(set[2]+ctxt@right.context))
  cpos.right <- cpos.right[which(cqi_cpos2struc(corpus.sattr, cpos.right)==set[3])]
  bag$cpos <- list(left=cpos.left, node=c(set[1]:set[2]), right=cpos.right)
  cpos <- c(cpos.left, cpos.right)
  if (!is.null(stopwordId)) ids <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pattribute, sep=""), cpos)
  posChecked <- cpos[.filter[[filterType]](cqi_cpos2str(paste(ctxt@corpus,".pos", sep=""), cpos), ctxt@posFilter)]
  bag$id <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pattribute, sep=""), posChecked)
  if (!is.null(stopwordId)) if (any(stopwordId %in% ids)) {bag <- NULL}
  bag
}



#' KWIC output
#' 
#' Based on a context object, you get concordances, i.e. the context of a 
#' keyword
#' 
#' This functiongives you quite some flexibility to adjust the output to your needs.
#' Use drillingControls to adjust output.
#' 
#' @param ctxt a context object
#' @param metadata character vector with the metadata included in output
#' @param collocate limit output to a concordances containing a specific 
#'   collocate
#' @return a concordances object
#' @author Andreas Blaette
#' @export kwic
kwic <- function(ctxt, metadata=NULL, collocate=c()){
  if(is.null(metadata)) metadata <- get("drillingControls", '.GlobalEnv')[['kwicMetadata']]
  m <- data.frame(dummy=rep(0, length(ctxt@cpos)))
  if (all(is.element(metadata, cqi_attributes(ctxt@corpus, "s")))!=TRUE) {
    warning("check drillingControls$kwicMetadata: Not all sAttributes supplied are available in corpus")
  }
  for (meta in metadata){
    sattr <- paste(ctxt@corpus, ".", meta, sep="")
    strucs <- cqi_cpos2struc(sattr, unlist(lapply(ctxt@cpos, function(x)x$node[1])))
    m <- cbind(m, cqi_struc2str(sattr, strucs))
  }
  left <- unlist(lapply(ctxt@cpos, function(x) {paste(cqi_cpos2str(paste(ctxt@corpus,'.', ctxt@pattribute, sep=""), x$left), collapse=" ")}))
  node <- unlist(lapply(ctxt@cpos, function(x) {paste(cqi_cpos2str(paste(ctxt@corpus,'.', ctxt@pattribute, sep=""), x$node), collapse=" ")}))
  right <- unlist(lapply(ctxt@cpos, function(x) {paste(cqi_cpos2str(paste(ctxt@corpus,'.', ctxt@pattribute, sep=""), x$right), collapse=" ")}))
  Encoding(left) <- ctxt@encoding
  Encoding(node) <- ctxt@encoding
  Encoding(right) <- ctxt@encoding  
  m <- cbind(m, left=left, node=node, right=right)
  if (length(collocate) > 0) m <- m[grep(collocate, apply(m, 1, function(x)paste(x[length(x)-2], x[length(x)]))),]
  m <- m[2:ncol(m)]
  colnames(m) <- c(metadata, c('left.context', 'node', 'right.context'))
  conc <- new('concordances')
  if (!is.null(collocate)) {conc@collocate <- collocate}
  conc@table <- m
  conc@metadata <- metadata
  conc@encoding <- ctxt@encoding
  conc
}




#' @importFrom xtermStyle style
setMethod('show', 'concordances',
function(object){
  drillingControls <- get("drillingControls", '.GlobalEnv')
  for (i in 1:nrow(object@table)){
    metaoutput <- paste(as.vector(unname(unlist(object@table[i,c(1:length(object@metadata))]))), collapse=" | ")
    Encoding(metaoutput) <- object@encoding
    if (drillingControls$xtermStyle==FALSE){
      cat('[',metaoutput, '] ', sep='')
    } else {
      cat(style(paste('[',metaoutput, ']',sep=''),fg=drillingControls$xtermFgMeta,bg=drillingControls$xtermBgMeta), ' ', sep='')
    }
    if (drillingControls$xtermStyle==FALSE){
    cat(paste(as.vector(unname(unlist(object@table[i,c((ncol(object@table)-2):ncol(object@table))]))), collapse=" * "), "\n\n")
    } else {
      if (length(object@collocate)==0){object@collocate="FOO"}
      foo <- sapply(unlist(strsplit(as.vector(unname(unlist(object@table[i,ncol(object@table)-2]))), ' ')),
                    function(x){
                      if (x==object@collocate){
                        cat(style(x, bg=drillingControls$xtermBgCollocate, fg=drillingControls$xtermFgCollocate), ' ')
                      } else {cat(x, ' ', sep='')}
                    }
      )
      cat(' ', style(object@table[i,ncol(object@table)-1], bg=drillingControls$xtermBgNode, fg=drillingControls$xtermFgNode), ' ', sep='')
      foo <- sapply(unlist(strsplit(as.vector(unname(unlist(object@table[i,ncol(object@table)]))), ' ')),
                    function(x){
                      if (x==object@collocate){
                        cat(style(x, bg=drillingControls$xtermBgCollocate, fg=drillingControls$xtermFgCollocate), ' ')
                      } else {cat(x, ' ', sep='')}
                    }
      )                                                     
      cat("\n\n")
    }
  }          
}
)

#' @exportMethod summary
#' @noRd
setMethod('summary', 'context',
function(object) {
  cat("\n** Context object - general information: **\n")
  cat(sprintf("%-20s", "CWB-Korpus:"), object@corpus, "\n")
  cat(sprintf("%-20s", "Partition:"), object@partition, "\n")
  cat(sprintf("%-20s", "Node:"), object@query, "\n")
  cat(sprintf("%-20s", "P-Attribute:"), object@pattribute, "\n")
  cat(sprintf("%-20s", "Node count:"), object@frequency, "\n")
  cat(sprintf("%-20s", "Stat table length:"), nrow(object@stat), "\n\n")

  cat("\n** Statistical summary: **\n")
  print(object@statisticalSummary)
  
  cat("\n** Top ten: **\n")
  print(object@stat[1:10,c(1,3,4,7)])
}
)

.statisticalSummary <- function(object) {
  if (object@statisticalTest %in% c("LL", "chiSquare")){
    criticalValue <- c(3.84, 6.63, 7.88, 10.83)
    propability <- c(0.05, 0.01, 0.005, 0.001)
     no <- vapply(
       criticalValue,
       function(x) length(which(object@stat[[object@statisticalTest]]>x)),
       FUN.VALUE=1
     )
    result <- data.frame(propability, criticalValue, no)
    result <- result[order(result$propability, decreasing=FALSE),]
  }
  return(result)
}

  

.showChunkwise <- function (conc) {
  drillingControls <- get("drillingControls", '.GlobalEnv')
  if (drillingControls$kwicNo == 0 ) {
    show(conc)
  } else if (drillingControls$kwicNo > 0) {
    chunks <- trunc(nrow(conc@table)/drillingControls$kwicNo)
    for ( i in c(0:(chunks-1))) {
      lines <- i*drillingControls$kwicNo+c(1:drillingControls$kwicNo)
      cat ('---------- KWIC output', min(lines), 'to', max(lines), 'of', nrow(conc@table),'----------\n\n')
      foo <- show(conc[lines])
      cat("(press 'q' to quit or ENTER to continue)\n")
      loopControl <- readline()
      if (loopControl == "q") break
    }
    if ((chunks*drillingControls$kwicNo < nrow(conc@table)) && (loopControl != "q")){
      cat ('---------- KWIC output', chunks*drillingControls$kwicNo, 'to', nrow(conc@table), 'of', nrow(conc@table),'----------\n\n')
      foo <- show(conc[c((chunks*drillingControls$kwicNo):nrow(conc@table))])
    }
  }    
}

setMethod('show', 'context',
function(object) {
  drillingControls <- get("drillingControls", '.GlobalEnv')
  conc <- kwic(object, metadata=drillingControls$kwicMetadata)
  if (nrow(conc@table) > drillingControls$kwicNo) {
    foo <- .showChunkwise(conc)
  } else {
    show(conc)
  }
})

setMethod('[', 'context',
          function(x,i) {
            drillingControls <- get("drillingControls", '.GlobalEnv')
            conc <- kwic(x, metadata=drillingControls$kwicMetadata)
            conc@table <- conc@table[i,]
            show(conc)
          }        
)

setMethod('[[', 'context',
  function(x,i) {
    drillingControls <- get("drillingControls", '.GlobalEnv')
    conc <- kwic(x, metadata=drillingControls$kwicMetadata, collocate=i)
    foo <- .showChunkwise(conc)
  }        
)

setMethod('[', 'concordances',
  function(x,i) {
    x@table <- x@table[i,]
    x
  }        
)


#' Prepare data for an ego-network
#' 
#' For a node word, collocates of n degrees are calculated
#' 
#' The function returns a data frame that can be converted into an igraph object easily.
#' This conversion is not part of the function to keep number of dependencies of the 
#' package low.
#' 
#' @param node query, which may by a multi-word unit
#' @param Partition a partition object
#' @param degrees the degrees of the resulting egoNetwork
#' @param pAttribute pattribute of the query
#' @param leftContext no of tokens and to the left of the node word
#' @param rightContext no of tokens to the right of the node word
#' @param minSignificance minimum log-likelihood value
#' @param posFilter character vector with the POS tags to be included - may not be empty!!
#' @return a data frame that can be turned into an igraph object with graph.data.frame (see example)
#' @examples
#' \dontrun{
#'  nw <- partition(list(text_year="2005", text_type="speech"), corpus="PLPRNWHTM")
#'  net <- egoNetwork(nw, "Integration", 1, "word", 5,5, 3.84, "NN")
#'  g <- graph.data.frame(net[,c(1,2,3,7)])
#'  tklplot(g)
#'  }
#' @author Andreas Blaette
#' @export egoNetwork
egoNetwork <- function(node, Partition, degrees, pAttribute="useControls", leftContext=0, rightContext=0, minSignificance, posFilter=c()) {
  gData <- context(node, Partition, pAttribute, leftContext, rightContext, minSignificance, posFilter)@stat
  gData <- cbind(node=rep(node, times=nrow(gData)), target=rownames(gData), degree=rep(1, times=nrow(gData)), gData)
  rownames(gData) <- NULL
  for ( degree in 2:degrees ) {
    terms <- gData[which(gData$degree==(degree-1)),2]
    for ( term in terms ) {
      dataNew <- context(term, Partition, pAttribute, leftContext, rightContext, minSignificance, posFilter)@stat
      dataNew <- cbind(node=rep(term, times=nrow(dataNew)), target=rownames(dataNew), degree=degree, dataNew)
      rownames(dataNew) <- NULL
      gData <- rbind(gData, dataNew)
    }
  }
  vertices <- unique(c(as.vector(unname(unlist(gData[,1]))), as.vector(unname(unlist(gData[,2])))))
  # verticeData <- data.frame(vertices=vertices, Partition@tf[[pAttribute]][vertices,"tf"])
  gData
}

#' combine statistics
#' 
#' Merge statistics for two contextual analyses, calculate pearson rank correlation (experimental)
#' 
#' The function is intended to facilitate the exploration of the variation of a term.
#' Setting the max.rank needs to be handled with care. 
#' This analysis makes only sense for dyachronic comparisons with roughly equal frequencies.
#' 
#' @param x context object 1
#' @param y context object 2
#' @param maxRank a cutoff rank
#' @param minFrequency a minimum frequency
#' @param tokenFilter keep rows only for tokens given by a character vector
#' @param pearson set to TRUE if pearsons rho shall be calculated, defaults to FALSE
#' @return a combined data frame
#' @author Andreas Blaette
#' @export combineCollocates
combineCollocates <- function (x, y, maxRank=0, minFrequency=0, tokenFilter=c("FOO"), pearson=FALSE){
  x <- trim(x, minFrequency=minFrequency, maxRank=maxRank)
  y <- trim(y, minFrequency=minFrequency, maxRank=maxRank)
  c <- merge(x@stat, y@stat, all=TRUE, by.x=0, by.y=0)
  rownames(c) <- c[,1]
  c <- c[,2:dim(c)[2]]
  c <- cbind(c,
             x.plot=sapply(c[,1], function(x) if (is.na(x)==TRUE) {maxRank} else {x}),
             y.plot=sapply(c[,5], function(x) if (is.na(x)==TRUE) {maxRank} else {x})
  )
  if (!unique(tokenFilter)[1]=="FOO") {c <- c[which(rownames(c)%in% tokenFilter),]}
  comparison <- list(
    partition.x=x@partition,
    partition.y=y@partition,
    node.x=x@query,
    node.y=y@query,
    stat=c
  )
  if (pearson==TRUE) {comparison$pearsons.rho=cor.test(c[,4], c[,8], method="pearson")$estimate}
  comparison
}



