setGeneric("partitionBundle", function(object, ...) standardGeneric("partitionBundle"))

#' Generate a list of partitions
#' 
#' A list of partition objects with fixed s-attributes and one variable
#' s-attribute is generated
#' 
#' @param object character string, the CWB corpus to be used
#' @param def a list with the definition of a partition that shall be prepared
#' @param var list indicating the s-attribute to be variabel
#' @param prefix a character vector that will serve as a prefix for partition names
#' @param encoding encoding of the corpus, if not provided, encoding provided in the registry file will be used
#' @param pAttribute the pAttributes for which term frequencies shall be retrieved
#' @param meta a character vector
#' @param regex logical
#' @param xml either 'flat' (default) or 'nested'
#' @param id2str whether to turn token ids to strings (set FALSE to minimize object.size / memory consumption)
#' @param type the type of the partition objects
#' @param mc logical, whether to use multicore parallelization
#' @param verbose logical, whether to provide progress information
#' @return a S4 class 'partitionBundle', which is a list with partition objects
#' @importFrom parallel mclapply
#' @export partitionBundle
#' @aliases partitionBundle partitionBundle,character-method
#' @author Andreas Blaette
#' @name partitionBundle
#' @docType methods
#' @rdname partitionBundle-method
setMethod("partitionBundle", "character", function(
  object, def, var, prefix=c(""),
  encoding=NULL, pAttribute=NULL, meta=NULL, regex=FALSE, xml="flat", id2str=TRUE, type=NULL, mc=NULL, verbose=TRUE
) {
  if (length(names(var))==1) {
    sAttributeVar <- names(var)
    sAttributeVarValues <- var[[sAttributeVar]]
  } else {
    warning("only one variable s-attribute may be provided")
  }
  if (is.null(mc)) mc <- slot(get("session", '.GlobalEnv'), 'multicore')
  multicoreMessage <- ifelse(
    mc==TRUE,
    ' (use multicore: TRUE)',
    ' (use multicore: FALSE)'
  )
  if (verbose==TRUE) message('\nPreparing bundle of partitions', multicoreMessage)
  bundle <- new(
    "partitionBundle",
    corpus=object,
    sAttributesFixed=def,
    call=deparse(match.call())
  )
  if (verbose==TRUE) message('... setting up base partition')
  partitionBase <- partition(object, def, pAttribute=c(), meta=meta, regex=regex, xml=xml, id2str=FALSE, type=type, verbose=FALSE)
  bundle@encoding <- partitionBase@encoding
  if (is.null(sAttributeVarValues)){
    if (verbose==TRUE) message('... getting values of fixed s-attributes')
    sAttributeVarValues <- unique(cqi_struc2str(paste(object, '.', sAttributeVar, sep=''), partitionBase@strucs))
    Encoding(sAttributeVarValues) <- bundle@encoding
    if (verbose==TRUE) message('... number of partitions to be initialized: ', length(sAttributeVarValues))
  }
  if (mc==FALSE) {
    for (sAttribute in sAttributeVarValues){
      sAttr <- list()
      sAttr[[sAttributeVar]] <- sAttribute
      bundle@objects[[sAttribute]] <- partition(partitionBase, def=sAttr, name=sAttribute, pAttribute=pAttribute, id2str=id2str, type=type)
    }
  } else if (mc==TRUE) {
    if (verbose==TRUE) message('... setting up the partitions')
    bundle@objects <- mclapply(
      sAttributeVarValues,
      function(x) partition(
        partitionBase,
        def=sapply(sAttributeVar, function(y) x, USE.NAMES=TRUE),
        name=x,
        pAttribute=pAttribute,
        id2str=id2str,
        type=type
      )
    )
  }
  names(bundle@objects) <- paste(.adjustEncoding(prefix, bundle@encoding), sAttributeVarValues, sep='')
  bundle
})

#' @rdname partitionBundle-method
setMethod("partitionBundle", "list", function(
  object, var, prefix=c(""), encoding=NULL, pAttribute=NULL, meta=NULL,
  regex=FALSE, xml="flat", id2str=TRUE, mc=FALSE, verbose=TRUE
) {
  partitionBundle(
    object=get('session', '.GlobalEnv')@corpus,
    def=object, var=var, prefix=prefix, encoding=encoding, pAttribute=pAttribute,
    meta=meta, regex=regex, xml=xml, id2str=id2str, mc=mc, verbose=verbose
  )
})


