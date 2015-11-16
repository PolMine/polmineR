#' get all cooccurrences in a partition
#' 
#' @param .Object a partition object
#' @param pAttribute p-attribute to define tokens (can be length > 1)
#' @param window no of tokens to the left and to the right of nodes
#' @param method statistical test to use (defaults to "ll")
#' @param keep list with tokens to keep
#' @param mc whether to use multicore
#' @return a cooccurrences-class object
#' @exportMethod cooccurrences
#' @docType methods
#' @author Andreas Blaette
#' @export cooccurrences
#' @name cooccurrences
#' @rdname cooccurrences
#' @examples
#' \dontrun{
#' bt17merkel <- partition("PLPRTXT", list(text_lp="17", text_type="speech", text_speaker="Angela Merkel"))
#' bt17merkelColl <- cooccurrences(bt17merkel)
#' }
setGeneric("cooccurrences", function(.Object, ...){standardGeneric("cooccurrences")})


#' @rdname cooccurrences
setMethod(
  "cooccurrences", "partition",
  function(.Object, pAttribute=c("word", "pos"), window=5, keep=list(pos=c("NN", "ADJA")), method="ll", mc=FALSE, progress=TRUE, verbose=TRUE, ...){
  if (identical(pAttribute, .Object@pAttribute) == FALSE) .Object <- enrich(.Object, tf=pAttribute)
  coll <- new(
    "cooccurrences",
    pAttribute=pAttribute, corpus=.Object@corpus, encoding=.Object@encoding,
    leftContext=window, rightContext=window, partitionSize=.Object@size, stat=data.table()
  )
  coll@call <- deparse(match.call())
  coll@partition <- strsplit(deparse(sys.call(-1)), "\\(|\\)|,")[[1]][2]
  pAttr <- sapply(pAttribute, function(x) paste(.Object@corpus,".", x, sep=""))
  aColsId <- setNames(paste("a_", pAttribute, "_id", sep=""), pAttribute)
  bColsId <- setNames(paste("b_", pAttribute, "_id", sep=""), pAttribute)
  aColsStr <- setNames(paste("a_", pAttribute, sep=""), pAttribute)
  bColsStr <- setNames(paste("b_", pAttribute, sep=""), pAttribute)
  
  keepId <- lapply(setNames(names(keep), names(keep)), function(x) cqi_str2id(pAttr[[x]], keep[[x]]))
  
  if (verbose == TRUE) message("... making windows with corpus positions")
  .makeWindows <- function(i){
    cposMin <- .Object@cpos[i,1]
    cposMax <- .Object@cpos[i,2]
    if (cposMin != cposMax){
      cposRange <- c(cposMin:cposMax)
      lapply(
        setNames(cposRange, cposRange),
        function(x) {
          cpos <- c((x-window):(x-1), (x+1):(x+window))
          cpos <- cpos[which(cpos > cposMin)]
          cpos[which(cpos <= cposMax)]
        })
    }
  }
  if (mc == FALSE){
    bag <- lapply(
      c(1:nrow(.Object@cpos)),
      function(i){
        if (progress == TRUE) .progressBar(i=i, total=nrow(.Object@cpos))
        .makeWindows(i)
      }
    )
    bCpos <- lapply(bag, function(x) lapply(names(x), function(y) rep(as.numeric(y), times=length(x[[y]]))))
  } else {
    noCores <- ifelse(is.numeric(mc), mc, getOption("mc.cores", 2L))
    bag <- mclapply(c(1:nrow(.Object@cpos)), .makeWindows, mc.cores=noCores)
    bCpos <- mclapply(
      bag,
      function(x) lapply(names(x), function(y) rep(as.numeric(y), times=length(x[[y]]))),
      mc.cores=noCores
      )
  }
  if (verbose == TRUE) message("... putting together data.table")
  DT <- data.table(
    a_cpos=unlist(bag),
    b_cpos=unlist(bCpos)
  )
  
  if (verbose == TRUE) message("... getting token ids")
  lapply(
    pAttribute, function(x){
      DT[, eval(aColsId[x]) := cqi_cpos2id(pAttr[[x]], DT[["a_cpos"]]), with=TRUE]
      DT[, eval(bColsId[x]) := cqi_cpos2id(pAttr[[x]], DT[["b_cpos"]]), with=TRUE]
    }
  )
  
  if (verbose == TRUE) message("... counting window size")
  contextDT <- DT[, nrow(.SD), by=c(eval(aColsId)), with=TRUE] 
  
  if (verbose == TRUE) message("... applying filter")
  for (x in names(keep)){
    DT <- DT[DT[[aColsId[x]]] %in% keepId[[x]]]
    DT <- DT[DT[[bColsId[x]]] %in% keepId[[x]]]
  }
  
  if (verbose == TRUE) message("... counting joint occurrence")
  TF <- DT[, nrow(.SD), by=c(eval(c(aColsId, bColsId))), with=TRUE] # not fast
  setnames(TF, "V1", "ab_tf")
  
  if (verbose == TRUE) message("... adding window size")
  setkeyv(contextDT, cols=aColsId)
  setkeyv(TF, cols=aColsId)
  TF <- contextDT[TF]
  setnames(TF, "V1", "window_size")
  
  if (verbose == TRUE) message("... doing things")
  lapply(
    c(1:length(pAttribute)),
    function(i){
      TF[, eval(aColsStr[i]) := cqi_id2str(pAttr[i], TF[[aColsId[i]]]) %>% as.utf8, with=TRUE]
      TF[, eval(bColsStr[i]) := cqi_id2str(pAttr[i], TF[[bColsId[i]]]) %>% as.utf8, with=TRUE]
      TF[, eval(aColsId[i]) := NULL]
      TF[, eval(bColsId[i]) := NULL]
    }
  )
  setkeyv(TF, cols=aColsStr)
  setkeyv(.Object@stat, cols=pAttribute)
  TF[, a_tf := .Object@stat[TF][["tf"]]]
  setkeyv(TF, cols=bColsStr)
  TF[, b_tf := .Object@stat[TF][["tf"]]]
  setcolorder(TF, c(aColsStr, bColsStr, "ab_tf", "a_tf", "b_tf", "window_size"))
  coll@stat <- TF
  if ("ll" %in% method) {
    message('... g2-Test')
    coll <- ll(coll, partitionSize=.Object@size)
    coll@stat <- setorderv(coll@stat, cols="ll", order=-1)
  }
  coll@stat[, rank := c(1:nrow(coll@stat))]
  setcolorder(coll@stat, c("rank", colnames(coll@stat)[-which(colnames(coll@stat) == "rank")]))
  coll
})

#' @rdname cooccurrences
setMethod(
  "cooccurrences", "partitionBundle",
  function(.Object, pAttribute="word", window=5, method="ll", keep=list(pos=c("ADJA", "NN")), mc=FALSE){
  bundle <- new(
    "cooccurrencesBundle",
    encoding=unique(vapply(.Object@objects, function(x) x@encoding, FUN.VALUE="character")),
    corpus=unique(vapply(.Object@objects, function(x) x@corpus, FUN.VALUE="character"))
    )
  if (mc == FALSE){
    bundle@objects <- lapply(
      setNames(.Object@objects, names(.Object@objects)),
      function(x) {
        message('Calculating cooccurrences for partition ', x@name)
        cooccurrences(x, pAttribute=pAttribute, window=window, method=method, filter=filter, pos=pos)
      })
    
  } else {
    bundle@objects <- mclapply(
      setNames(.Object@objects, names(.Object@objects)),
      function(x) {
        message('Calculating cooccurrences for partition ', x@name)
        cooccurrences(
          x, pAttribute=pAttribute, window=window, method=method, filter=filter, pos=pos, mc=FALSE, progress=FALSE
          )
      }, mc.cores=slot(get('session', '.GlobalEnv'), "cores"))    
  }
  bundle
})





