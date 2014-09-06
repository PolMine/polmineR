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
#'   \item{html}{bla}
#'   \item{as.sparseMatrix}{bla}  
#'   }
#'   
#' @aliases partitionCluster-class show,partitionCluster-method 
#'   [,partitionCluster-method [[,partitionCluster-method
#'   as.DocumentTermMatrix,partitionCluster-method 
#'   as.matrix,partitionCluster-method 
#'   as.TermDocumentMatrix,partitionCluster-method merge,partitionCluster-method
#'   as.sparseMatrix,partitionCluster-Method as.sparseMatrix 
#'   +,partitionCluster-method names,partitionCluster-method 
#'   summary,partitionCluster-method +,partitionCluster,ANY-method
#'   [,partitionCluster,ANY,ANY,ANY-method +,partitionCluster,partition-method 
#'   +,partitionCluster,partitionCluster-method as.partitionCluster,list-method 
#'   as.sparseMatrix,partitionCluster-method as.TermDocumentMatrix as.DocumentTermMatrix
#'   addPos,partitionCluster-method barplot,partitionCluster-method
#'   as.sparseMatrix,TermDocumentMatrix-method
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
#' @param prefix a character vector that will serve as a prefix for partition labels
#' @param encoding encoding of the corpus, if not provided, encoding provided in the registry file will be used
#' @param tf the pAttributes for which term frequencies shall be retrieved
#' @param meta a character vector
#' @param method either 'grep' or 'in'
#' @param xml either 'flat' (default) or 'nested'
#' @param mc logical, whether to use multicore parallelization
#' @param verbose logical, whether to provide progress information
#' @return a S3 class 'partitionCluster', which is a list with partition objects
#' @importFrom parallel mclapply
#' @export partitionCluster
#' @aliases partitionCluster
#' @author Andreas Blaette
partitionCluster <- function(
  corpus, def, var, prefix=c(""),
  encoding=NULL, tf=c("word", "lemma"), meta=NULL, method="grep", xml="flat", mc=NULL, verbose=TRUE
) {
  if (length(names(var))==1) {
    sAttributeVar <- names(var)
    sAttributeVarValues <- var[[sAttributeVar]]
  } else {
    warning("only one variable s-attribute may be provided")
  }
  if (is.null(mc)) mc <- get("drillingControls", '.GlobalEnv')[['multicore']]
  multicoreMessage <- ifelse(
    mc==TRUE,
    ' (use multicore: TRUE)',
    ' (use multicore: FALSE)'
  )
  if (verbose==TRUE) message('\nPreparing cluster of partitions', multicoreMessage)
  cluster <- new("partitionCluster")
  cluster@corpus <- corpus
  cluster@sAttributesFixed <- def
  if (verbose==TRUE) message('... setting up base partition')
  partitionBase <- partition(corpus, def, tf=c(), meta=meta, method=method, xml=xml, verbose=FALSE)
  cluster@encoding <- partitionBase@encoding
  if (is.null(sAttributeVarValues)){
    if (verbose==TRUE) message('... getting values of fixed s-attributes')
    sAttributeVarValues <- unique(cqi_struc2str(paste(corpus, '.', sAttributeVar, sep=''), partitionBase@strucs))
    Encoding(sAttributeVarValues) <- cluster@encoding
    if (verbose==TRUE) message('... number of partitions to be initialized: ', length(sAttributeVarValues))
  }
  if (mc==FALSE) {
    for (sAttribute in sAttributeVarValues){
      sAttr <- list()
      sAttr[[sAttributeVar]] <- sAttribute
      cluster@partitions[[sAttribute]] <- zoom(partitionBase, def=sAttr, label=sAttribute, tf=tf)
    }
  } else if (mc==TRUE) {
    if (verbose==TRUE) message('... setting up the partitions')
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
