#' @include partition.R methods.R
NULL

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
#'     \item{as.data.frame}{get the statistics table}
#'    }
#'     
#' @name context-class
#' @aliases show,context-method [,context-method [,context,ANY,ANY,ANY-method [[,context-method summary,context-method
#' @docType class
#' @exportClass kwic
#' @rdname context-class
setClass("context",
         representation(query="character",
                        frequency="numeric",
                        partition="character",
                        partitionSize="numeric",
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



.surrounding <- function (set, ctxt, corpus.sattr, filterType, stoplistIds, positivelistIds) {
  bag <- list()
  set <- as.numeric(set)
  cpos.left <- c((set[1]-ctxt@left.context):(set[1]-1))
  cpos.left <- cpos.left[which(cqi_cpos2struc(corpus.sattr, cpos.left)==set[3])]
  cpos.right <- c((set[2]+1):(set[2]+ctxt@right.context))
  cpos.right <- cpos.right[which(cqi_cpos2struc(corpus.sattr, cpos.right)==set[3])]
  bag$cpos <- list(left=cpos.left, node=c(set[1]:set[2]), right=cpos.right)
  cpos <- c(cpos.left, cpos.right)
  posChecked <- cpos[.filter[[filterType]](cqi_cpos2str(paste(ctxt@corpus,".pos", sep=""), cpos), ctxt@posFilter)]
  bag$id <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pattribute, sep=""), posChecked)
  if (!is.null(stoplistIds) || !is.null(positivelistIds)) {
    ids <- cqi_cpos2id(paste(ctxt@corpus,".", ctxt@pattribute, sep=""), cpos)
    if (!is.null(stoplistIds)) if (any(stoplistIds %in% ids)) {bag <- NULL}
    if (!is.null(positivelistIds)) if (any(positivelistIds %in% ids) == FALSE) {bag <- NULL}
  }
  bag
}








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


  

setMethod('show', 'context',
function(object) {
  drillingControls <- get("drillingControls", '.GlobalEnv')
  conc <- kwic(object, metadata=drillingControls$kwicMetadata)
  show(conc)
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
    foo <- show(conc)
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

setMethod("context", "partitionCluster", function(
  object, query, pAttribute="useControls",
  leftContext=0, rightContext=0,
  minSignificance=-1, posFilter="useControls", filterType="useControls",
  stoplist=c(), statisticalTest="LL",
  verbose=TRUE  
) {
  contextCluster <- new("contextCluster")
  contextCluster@query <- query
  contextCluster@pAttribute <- pAttribute
  contextCluster@contexts <- sapply(
    partitionCluster@partitions,
    function(x) context(
      query, x,
      pAttribute=pAttribute,
      leftContext=leftContext, rightContext=rightContext,
      minSignificance=minSignificance, posFilter=posFilter, filterType=filterType,
      stoplist=stoplist, statisticalTest=statisticalTest,
      verbose=verbose
    ),
    simplify = TRUE,
    USE.NAMES = TRUE
  )
  contextCluster
})

