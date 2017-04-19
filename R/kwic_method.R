#' @include partition_class.R context_class.R
NULL 

#' KWIC output / concordances
#' 
#' Prepare and show concordances / keyword-in-context (kwic). The same result can be achieved by 
#' applying the kwich method on either a partition or a context object.
#' 
#' @param .Object a \code{partition} or \code{context} object
#' @param query a query, CQP-syntax can be used
#' @param cqp either logical (TRUE if query is a CQP query), or a
#'   function to check whether query is a CQP query or not (defaults to is.query
#'   auxiliary function)
#' @param left to the left
#' @param right to the right
#' @param meta metainformation to display
#' @param cpos logical, if TRUE, the corpus positions ("cpos") if the hits will be handed over to the kwic-object that is returned
#' @param pAttribute p-attribute, defaults to 'word'
#' @param sAttribute if provided, the s-attribute will be used to check the boundaries of the text
#' @param neighbor only show kwic if a certain word is present
#' @param verbose logical, whether to be talkative
#' @param ... further parameters to be passed
#' @rdname kwic
#' @docType methods
#' @seealso To read the whole text, see the \code{\link{read}}-method.
#' @examples
#' \dontrun{
#' if (require(polmineR.sampleCorpus) && require(rcqp)){
#'   use("polmineR.sampleCorpus")
#'   bt <- partition("PLPRBTTXT", def=list(text_date=".*"), regex=TRUE)
#'   kwic(bt, "Integration")
#'   kwic(
#'     bt, "Integration",
#'     left=20, right=20,
#'     meta=c("text_date", "text_name", "text_party")
#'   )
#'   kwic(
#'     bt, '"Integration" [] "(Menschen|Migrant.*|Personen)"',
#'     left=20, right=20,
#'     meta=c("text_date", "text_name", "text_party")
#'   ) 
#' }
#' }
#' @exportMethod kwic
setGeneric("kwic", function(.Object, ...){standardGeneric("kwic")})


#' @exportMethod kwic
#' @docType methods
#' @rdname kwic
setMethod("kwic", "context", function(.Object, meta = getOption("polmineR.meta"), cpos = TRUE, neighbor = NULL, verbose=FALSE){
  
  DT <- copy(.Object@cpos) # do not accidentily store things
  setorderv(DT, cols = c("hit_no", "cpos"))
  decoded_pAttr <- CQI$id2str(
    .Object@corpus, .Object@pAttribute[1],
    DT[[paste(.Object@pAttribute[1], "id", sep = "_")]]
    )
  decoded_pAttr2 <- as.nativeEnc(decoded_pAttr, from = .Object@encoding)
  DT[, .Object@pAttribute[1] := decoded_pAttr2, with = TRUE]
  DT[, "direction" := sign(DT[["position"]]), with = TRUE]
  
  if (length(neighbor) > 0){
    leftRightDT <- DT[which(DT[["direction"]] != 0)]
    hitsToKeep <- leftRightDT[grep(neighbor, leftRightDT[[.Object@pAttribute[1]]])][["hit_no"]]
    DT <- DT[which(DT[["hit_no"]] %in% hitsToKeep)]
    if (length(hitsToKeep) > 0 && cpos == TRUE){
      .Object@cpos <- DT
    } else {
      .Object@cpos <- data.table()
    }
  } 
  
  # paste left and right context
  if (nrow(DT) > 0){
    .paste <- function(.SD) paste(.SD[["word"]], collapse = " ")
    DT2 <- DT[, .paste(.SD), by = c("hit_no", "direction"), with = TRUE]
    tab <- dcast(data = DT2, formula = hit_no~direction, value.var = "V1")
    setnames(tab, old = c("-1", "0", "1"), new = c("left", "node", "right"))
  } else {
    tab <- data.table(hit_no = integer(), left = character(), node = character(), right = character())
  }
  
  
  if (is.null(meta)) meta <- character()
  conc <- new(
    'kwic',
    corpus = .Object@corpus, left = .Object@left, right = .Object@right,
    table = as.data.frame(tab),
    metadata = if (length(meta) == 0) character() else meta,
    encoding = .Object@encoding,
    cpos = if (cpos) DT else data.table()
    )

  conc <- enrich(conc, meta)
  if (!is.null(neighbor) || !length(neighbor) == 0) conc@neighbor <- neighbor
  
  conc
})


#' @rdname kwic
#' @exportMethod kwic
setMethod("kwic", "partition", function(
  .Object, query, cqp = is.cqp,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  meta = getOption("polmineR.meta"),
  pAttribute = "word", sAttribute = NULL, cpos = TRUE,
  neighbor = NULL,
  verbose = TRUE
){
  ctxt <- context(
    .Object = .Object, query = query, cqp = cqp,
    pAttribute = pAttribute, sAttribute = sAttribute,
    left = left, right = right,
    count = FALSE, verbose = verbose
  )
  if (is.null(ctxt)){
    message("... no occurrence of query")
    return(NULL)
    }
  kwic(.Object = ctxt, meta = meta, neighbor = neighbor, cpos = cpos)
})


#' @rdname kwic
setMethod("kwic", "character", function(
  .Object, query, cqp = is.cqp,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  meta = getOption("polmineR.meta"),
  pAttribute = "word", sAttribute = NULL, cpos = TRUE,
  neighbor = NULL,
  verbose = TRUE
){
  hits <- cpos(.Object, query = query, cqp = cqp, pAttribute = pAttribute, verbose = FALSE)
  if (is.null(hits)) {
    message("sorry, not hits")
    return(NULL)
  }
  cposMax <- CQI$attribute_size(.Object, pAttribute, type = "p")
  cposList <- apply(
    hits, 1,
    function(row){
      left <- c((row[1] - left - 1):(row[1] - 1))
      right <- c((row[2] + 1):(row[2] + right + 1))
      list(
        left = left[left > 0],
        node = c(row[1]:row[2]),
        right = right[right <= cposMax]
        )
    }
    )
  DT <- data.table(
    hit_no = unlist(lapply(1:length(cposList), function(i) rep(i, times = length(unlist(cposList[[i]]))))),
    cpos = unname(unlist(cposList)),
    position = unlist(lapply(
      cposList,
      function(x) lapply(
        names(x),
        function(x2)
          switch(
            x2,
            left = rep(-1, times = length(x[[x2]])),
            node = rep(0, times = length(x[[x2]])),
            right = rep(1, times = length(x[[x2]])))
        )))
  )
  DT[[paste(pAttribute, "id", sep = "_")]] <- CQI$cpos2id(.Object, pAttribute, DT[["cpos"]])
  
  ctxt <- new(
    "context",
    count = nrow(hits), stat = data.table(),
    corpus = .Object,
    left = left, right = right, 
    cpos = DT,
    pAttribute = pAttribute,
    encoding = RegistryFile$new(.Object)$getEncoding()
    )
  if (!is.null(sAttribute)) ctxt@sAttribute <- sAttribute
  kwic(.Object = ctxt, meta = meta, neighbor = neighbor, cpos = cpos)
})

