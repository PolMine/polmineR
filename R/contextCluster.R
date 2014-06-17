#' S4 contextCluster class
#' 
#' class to organize information of multiple context analyses
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{contexts}:}{Object of class \code{"list"} a list of context objects }
#'   }
#' @section Methods:
#'   \describe{
#'     \item{show}{output of core information}
#'     \item{summary}{core statistical information}
#'     \item{[}{specific collocates}
#'     \item{[[}{specific collocates}
#'    }
#'     
#' @name contextCluster-class
#' @aliases show,contextCluster-method summary,contextCluster-method [,contextCluster-method [,contextCluster,ANY,ANY,ANY-method [[,contextCluster-method as.TermContextCluster,contextCluster-method as.TermContextMatrix
#' @docType class
#' @exportClass kwic
#' @rdname contextCluster-class
setClass("contextCluster",
         representation(
           contexts="list",
           query="character",
           pAttribute="character"
         )
)

#' @include context.R generics.R partitionCluster.R
setMethod("context", "partitionCluster", function(
  object, query, pAttribute="useControls",
  leftContext=0, rightContext=0,
  minSignificance=-1, posFilter="useControls", filterType="useControls",
  stopwords=c(), statisticalTest="LL",
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
      stopwords=stopwords, statisticalTest=statisticalTest,
      verbose=verbose
    ),
    simplify = TRUE,
    USE.NAMES = TRUE
  )
  contextCluster
})

setMethod("[[", "contextCluster", function(x,i){
  return(x@contexts[[i]])
})

setMethod("[", "contextCluster", function(x,i){
  tf <- unlist(lapply(x, function(x) x@stat[i,"freqObs"]))
  cat("be aware of bugs\n")
})

#' @exportMethod summary
#' @noRd
setMethod('summary', 'contextCluster',
          function(object) {
            cat("\n** ContextCluster object - general information: **\n")
            cat(sprintf("%-20s", "Node:"), object@query, "\n")
            cat(sprintf("%-20s", "P-Attribute:"), object@pAttribute, "\n")
            cat("\n** Statistical summary: **\n")
            yet("yet to be implemented\n")      
          }
)


#' @exportMethod summary
#' @noRd
setMethod('show', 'context', function(object) {
            return(summary(object))
})


as.TermContextMatrix <- function(x, col, ...) UseMethod("as.TermContextMatrix", x)

#' Transform a context cluster into a Term Context Matrix
#' 
#' Method based on the tm package, adds to as.TermDocumentMatrix
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a contextCluster object (S3 class)
#' @param col the col of the stat table to take
#' @param ... to make the check happy
#' @method as.TermContextMatrix contextCluster
#' @return a TermContextMatrix
#' @author Andreas Blaette
#' @docType method
#' @importFrom slam simple_triplet_matrix
#' @exportMethod as.TermContextMatrix
#' @noRd
setMethod("as.TermContextMatrix", "contextCluster", function (x, col, ...) {
  encoding <- unique(unlist(lapply(x@contexts, function(c) c@encoding)))
  corpus <- unique(unlist(lapply(x@contexts, function(c) c@corpus)))
  pAttribute <- unique(unlist(lapply(x@contexts, function(c) c@pattribute)))
  pAttr <- paste(corpus, '.', pAttribute, sep='')
  i <- unlist(lapply(x@contexts, function(c) (cqi_str2id(pAttr, rownames(c@stat))+1)))
  j <- unlist(lapply(c(1:length(x@contexts)), function(m) {rep(m,times=nrow(x[[m]]@stat))}))
  v <- unlist(lapply(x@contexts, function(c) c@stat[,col]))
  lexiconSize <- cqi_lexicon_size(pAttr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x@contexts),
                               nrow=lexiconSize+1,
                               dimnames=list(
                                 Terms=cqi_id2str(pAttr, c(0:lexiconSize)),
                                 Docs=names(x@contexts))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
  class(mat) <- c("TermContextMatrix", "TermDocumentMatrix", "simple_triplet_matrix")
  mat
})

#' Turn a context cluster into a matrix
#' 
#' Method based on the tm package.
#' @param x a contextCluster object (S3 class)
#' @param col the to be used
#' @param ... furhter arguments
#' @method as.matrix contextCluster
#' @return a matrix
#' @author Andreas Blaette
#' @exportMethod as.matrix
#' @noRd
setMethod("as.matrix", "contextCluster", function(x, col, ...) {
  slamStyle <- as.TermContextMatrix(x, col)
  mat <- as.matrix(slamStyle)
  mat <- mat[which(rowSums(mat)>0),]
  mat
})

setMethod("summary", "contextCluster", function(object, top=3){
  partitionSizes=unlist(lapply(object@contexts, function(x) x@partitionSize))
  tfAbs=unlist(lapply(object@contexts, function(x) x@frequency))
  overview <- data.frame(
    tfAbs=tfAbs,
    tfRel=round(tfAbs/partitionSizes*100000,2)
    )
  overview <- cbind(overview, t(data.frame(lapply(object@contexts, function(x) .statisticalSummary(x)$no))))
  colnames(overview)[3:6] <- criticalValue <- c(">10.83", ">7.88", ">6.63", ">3.84")
  overview <- cbind(overview, t(data.frame(lapply(object@contexts, function(x) rownames(x@stat)[1:top]))))
  overview
})

setMethod("show", "contextCluster", function(object){
  summary(object)
})