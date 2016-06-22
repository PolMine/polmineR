#' get all cooccurrences in a partition
#' 
#' @param .Object a partition object
#' @param window no of tokens to the left and to the right of nodes
#' @param method statistical test to use (defaults to "ll")
#' @param verbose logical, whether to be verbose
#' @param progress logical, whether to be verbose
#' @param keep list with tokens to keep
#' @param big logical, whether to use bigmatrix
#' @param matrix logical, whether to return matrix
#' @param mc whether to use multicore
#' @param ... further parameters that will be passed into bigmatrix (applies only of big=TRUE)
#' @return a cooccurrences-class object
#' @exportMethod cooccurrences
#' @docType methods
#' @author Andreas Blaette
#' @export cooccurrences
#' @name cooccurrences
#' @rdname cooccurrences
#' @examples
#' if (require(polmineR.sampleCorpus) && require(rcqp)){
#'   merkel <- partition("PLPRBTTXT", text_type="speech", text_name=".*Merkel", regex=TRUE)
#'   merkel <- enrich(merkel, pAttribute="word")
#'   cooc <- cooccurrences(merkel)
#' }
setGeneric("cooccurrences", function(.Object, ...){standardGeneric("cooccurrences")})


#' @rdname cooccurrences
setMethod(
  "cooccurrences", "partition",
  function(.Object, window=5, keep=list(pos=c("NN", "ADJA")), method="ll", big=FALSE, matrix=FALSE, mc=FALSE, progress=TRUE, verbose=TRUE, ...){
    if (require("rcqp", quietly = TRUE)){
      pAttribute <- .Object@pAttribute
      coll <- new(
        "cooccurrences",
        pAttribute=pAttribute, corpus=.Object@corpus, encoding=.Object@encoding,
        left=window, right=window, partitionSize=.Object@size, stat=data.table()
      )
      coll@call <- deparse(match.call())
      coll@partition <- strsplit(deparse(sys.call(-1)), "\\(|\\)|,")[[1]][2]
      pAttr <- sapply(pAttribute, function(x) paste(.Object@corpus,".", x, sep=""))
      aColsId <- setNames(paste("a_", pAttribute, "_id", sep=""), pAttribute)
      bColsId <- setNames(paste("b_", pAttribute, "_id", sep=""), pAttribute)
      aColsStr <- setNames(paste("a_", pAttribute, sep=""), pAttribute)
      bColsStr <- setNames(paste("b_", pAttribute, sep=""), pAttribute)
      
      # turn tokens to keep to id 
      if (all(pAttribute %in% names(keep))){
        keepId <- lapply(setNames(names(keep), names(keep)), function(x) cqi_str2id(pAttr[[x]], keep[[x]]))  
      }
      
      
      if (big == TRUE){
        if (requireNamespace("bigmemory", quietly = TRUE) && requireNamespace("bigtabulate", quietly = TRUE) ) {
          if (verbose == TRUE) message("... generating context tables")
          BIG <- bigmemory::big.matrix(ncol = window * 2 + 1, nrow = .Object@size, ...)
          ids <- lapply(
            c(1:nrow(.Object@cpos)),
            function(i) 
              cqi_cpos2id(paste(.Object@corpus, pAttribute, sep="."), c(.Object@cpos[i,1]: .Object@cpos[i,2]))
          )
          idPos <- cumsum(lapply(ids, length))
          mclapply(
            c(1:length(ids)),
            function(i){
              idChunk <- ids[[i]]
              lapply(
                c(-window:-1, 1:window),
                function(x){
                  idsToFill <- c(
                    rep(NA, times=min(ifelse( x<0 , -x, 0), length(idChunk))),
                    idChunk[
                      ifelse(length(idChunk) <= abs(x), 0, ifelse(x<0, 1, x+1))
                      :
                        ifelse(length(idChunk) <= abs(x), 0, ifelse(x<0, length(idChunk)+x, length(idChunk)))
                      ],
                    rep(NA, times=min(ifelse(x>0, x, 0), length(idChunk)))
                  )
                  BIG[c(ifelse(i == 1, 1, idPos[i-1]+1):idPos[i]), ifelse(x <0, x + window + 1, x+window)] <- idsToFill
                  BIG[c(ifelse(i == 1, 1, idPos[i-1]+1):idPos[i]), window * 2 + 1] <- idChunk
                })
            }, mc.cores=ifelse(is.numeric(mc), mc, 1))
          if (verbose == TRUE) message("... counting cooccurrences")
          rowIndices <- bigtabulate::bigsplit(BIG, ccols=ncol(BIG), breaks=NA, splitcol=NA)
          tables <- mclapply(
            names(rowIndices),
            function(node){
              toTabulate <- as.vector(BIG[rowIndices[[node]], c(1:(window * 2))])
              toTabulate <- toTabulate + 1
              tabulated <- tabulate(toTabulate)
              idRawPresent <- which(tabulated != 0)
              matrix(
                data=c(
                  rep(as.integer(node), times=length(idRawPresent)),
                  idRawPresent - 1,
                  tabulated[idRawPresent],
                  rep(sum(tabulated[idRawPresent]), times=length(idRawPresent))
                ),
                ncol = 4
              )
            }, mc.cores=ifelse(is.numeric(mc), mc, 1)
          )
          rm(BIG)
          countMatrices <- do.call(rbind, tables)
          TF <- data.table(countMatrices)
          setnames(TF, c(aColsId[1], bColsId[1], "count_ab", "size_window"))
        } else {
          stop("MISSING DEPENDENCIES: Packages bigmemory and/or bigtabulate are not installed") 
        }
        
        
      } else if (big == FALSE){
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
        if (all(pAttribute %in% names(keep))){
          for (x in names(keep)){
            DT <- DT[DT[[aColsId[x]]] %in% keepId[[x]]]
            DT <- DT[DT[[bColsId[x]]] %in% keepId[[x]]]
          }
        }
        
        if (verbose == TRUE) message("... counting co-occurrences")
        TF <- DT[, nrow(.SD), by=c(eval(c(aColsId, bColsId))), with=TRUE] # not fast
        setnames(TF, "V1", "count_ab")
        
        if (verbose == TRUE) message("... adding window size")
        setkeyv(contextDT, cols=aColsId)
        setkeyv(TF, cols=aColsId)
        TF <- contextDT[TF]
        setnames(TF, "V1", "size_window")
      }
      
      if (verbose == TRUE) message("... converting ids to strings")
      lapply(
        c(1:length(pAttribute)),
        function(i){
          TF[, eval(aColsStr[i]) := as.utf8(cqi_id2str(pAttr[i], TF[[aColsId[i]]])), with=TRUE]
          TF[, eval(bColsStr[i]) := as.utf8(cqi_id2str(pAttr[i], TF[[bColsId[i]]])), with=TRUE]
          TF[, eval(aColsId[i]) := NULL]
          TF[, eval(bColsId[i]) := NULL]
        }
      )
      setkeyv(TF, cols=aColsStr)
      setkeyv(.Object@stat, cols=pAttribute)
      TF[, "count_a" := .Object@stat[TF][["count"]]]
      setkeyv(TF, cols=bColsStr)
      TF[, "count_b" := .Object@stat[TF][["count"]]]
      setcolorder(TF, c(aColsStr, bColsStr, "count_ab", "count_a", "count_b", "size_window"))
      coll@stat <- TF
      if ("ll" %in% method) {
        message('... g2-Test')
        coll <- ll(coll)
        coll@stat <- setorderv(coll@stat, cols="ll", order=-1)
      }
      if (matrix == FALSE){
        return(coll)
      } else if (matrix != FALSE){
        concatenate <- function(x) paste(x, collapse="//")
        TF[, "strKeyA" := apply(TF[, eval(paste("a_", pAttribute, sep="")), with=FALSE], 1, concatenate)]
        TF[, "strKeyB" := apply(TF[, eval(paste("b_", pAttribute, sep="")), with=FALSE], 1, concatenate)]
        uniqueKey <- unique(c(TF[["strKeyA"]], TF[["strKeyB"]]))
        keys <- setNames(c(1:length(uniqueKey)), uniqueKey)
        i <- unname(keys[TF[["strKeyA"]]])
        j <- unname(keys[TF[["strKeyB"]]])
        retval <- simple_triplet_matrix(
          i=i, j=j, v=TF[["count_ab"]],
          dimnames=list(a=names(keys)[1:max(i)], b=names(keys)[1:max(j)])
        )
        return(retval)
      } else {
        message("rcqp needs to be available")
      }
    }
  })

#' @rdname cooccurrences
setMethod("cooccurrences", "partitionBundle", function(.Object, mc=FALSE, ...){
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
        cooccurrences(x, ...)
      })
    
  } else {
    bundle@objects <- mclapply(
      setNames(.Object@objects, names(.Object@objects)),
      function(x) {
        message('Calculating cooccurrences for partition ', x@name)
        cooccurrences(x, ...)
      }, mc.cores=getOption("polmineR.cores"))    
  }
  bundle
})

