#' Get cooccurrence statistics.
#' 
#' @param .Object a partition object
#' @param window no of tokens to the left and to the right of nodes
#' @param cpos integer vector with corpus positions, defaults to NULL - then the corpus positions for the whole corpus will be used
#' @param pAttribute the pAttribute of the tokens
#' @param method statistical test to use (defaults to "ll")
#' @param verbose logical, whether to be verbose
#' @param progress logical, whether to be verbose
#' @param keep list with tokens to keep
#' @param big logical, whether to use bigmatrix
#' @param tcm logical, if TRUE, a term-cooccurrence matrix (sparse matrix, simple_triplet_matrix) will be returned
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
#' \dontrun{
#'   use(polmineR.sampleCorpus)
#'   merkel <- partition("PLPRBTTXT", text_type = "speech", text_name = ".*Merkel", regex = TRUE)
#'   merkel <- enrich(merkel, pAttribute = "word")
#'   cooc <- cooccurrences(merkel, keep = NULL)
#'   cooc <- cooccurrences(merkel, keep = NULL, big = TRUE)
#' }
setGeneric("cooccurrences", function(.Object, ...) standardGeneric("cooccurrences") )

#' @rdname cooccurrences
setMethod("cooccurrences", "character", function(.Object, keep = NULL, cpos = NULL, pAttribute = "word", window = 5, method = "new", verbose = TRUE){
  startTime <- Sys.time()
  # somewhat slow, consider cwb-decode 
  if (is.null(cpos)) cpos <- 0:(size(.Object) - 1)
  if (verbose) message("... getting ids")
  ids <- CQI$cpos2id(.Object, pAttribute, cpos)
  if (!is.null(keep)){
    if (verbose) message("... removing ids not to keep")
    if (is.character(keep)) keep <- CQI$str2id(.Object, pAttribute, iconv(x = keep, from = "utf-8", to = getEncoding(.Object)))
    ids[which(!ids %in% keep)] <- NA
  }
  if (method == "new"){
    if (verbose) message("... creating matrix columns")
    columns <- lapply(
      -window:window,
      function(i){
        if( i < 0 ){
          return( c(rep(NA, times = abs(i)), ids[1:(length(ids) + i)]) )
        } else {
          return( c(ids[(i + 1):(length(ids))], rep(NA, times = i)) )
        }
      }
    )
    if (verbose) message("... creating matrix")
    M <- do.call(cbind, columns)
    rm(columns)
    if (verbose) message("... as.data.table")
    DT <- as.data.table(M)
    rm(M)
    gc()
    setnames(DT, old = paste("V", window + 1, sep = ""), new = "node")
    if (verbose) message("... melting")
    DT2 <- data.table::melt.data.table(DT, id.vars = "node", value.name = "cooc", na.rm = TRUE)
    rm(DT)
    gc()
    DT2[, "variable" := NULL, with = TRUE]
    if (verbose) message("... kicking out unwanted terms")
    DT3 <- DT2[-which(is.na(DT2[["node"]]))]
    rm(DT2)
    # DT4 <- DT3[-which(is.na(DT3[["cooc"]]))]
    if (verbose) message("... counting cooccurrences")
    DT4 <- DT3[, .N, by = c("node", "cooc"), with = TRUE]
    rm(DT3)
    ID2STR <- data.table(
      id = keep,
      str = as.utf8(CQI$id2str(.Object, "word", keep))
    )
    setkeyv(ID2STR, cols = "id")
    setorderv(ID2STR, cols = "id")
    ID2STR[, "id_new" := 1:nrow(ID2STR), with = TRUE]
    setkeyv(DT4, "node")
    if (verbose) message("... foo1")
    DT5 <- DT4[ID2STR]
    rm(DT4)
    data.table::setnames(DT5, old = c("str", "id_new"), new = c("node_token", "node_new_key"))
    setkeyv(DT5, "cooc")
    if (verbose) message("... foo2")
    DT6 <- DT5[ID2STR]
    rm(DT5)
    setnames(DT6, old = "id_new", new = "cooc_new_key")
    if (verbose) message("... foo3")
    retval <- slam::simple_triplet_matrix(
      i = DT6[["node_new_key"]],
      j = DT6[["cooc_new_key"]],
      v = DT6[["N"]],
      dimnames = list(ID2STR[["str"]], ID2STR[["str"]])
    )
    return(retval)
  } else {
    iter <- c(-window:-1, 1:window)
    i_vector <- unlist(lapply(iter, function(i) if(i < 0) return((abs(i) + 1):length(ids)) else return(1:(length(ids) - i)) ))
    j_vector <- unlist(lapply(iter, function(i) rep(i + window + 1, times = (length(ids) - abs(i)) )))
    v_vector <- unlist(lapply(iter, function(i) if(i < 0) return(ids[1:(length(ids) + i)]) else return(ids[(i + 1):(length(ids))])))
    sparse_matrix <- simple_triplet_matrix(i = i_vector, j = j_vector, v = v_vector)
    rm(i_vector, j_vector, v_vector)
    dense_matrix <- as.matrix(sparse_matrix)
    coocs <- split(x = dense_matrix, f = ids)
    rm(sparse_matrix, dense_matrix)
    node_vector <- unlist(Map(
      f = function(i, j) rep(i, times = j),
      as.integer(names(coocs)),
      unlist(lapply(coocs, length))
    ))
    cooc_vector <- unlist(coocs, recursive = FALSE, use.names = FALSE)
    DT <- data.table(node = node_vector, cooc = cooc_vector)
    rm(node_vector, cooc_vector)
    DT2 <- DT[, .N, by = c("node", "cooc"), with = TRUE]
    
  }
  message(Sys.time() - startTime)
  DT2
})

#' @rdname cooccurrences
setMethod(
  "cooccurrences", "partition",
  function(.Object, window = 5, keep = list(pos = c("NN", "ADJA")), method = "ll", big = FALSE, tcm = FALSE, mc = FALSE, progress = TRUE, verbose = TRUE, ...){
    if (require("rcqp", quietly = TRUE)){
      pAttribute <- .Object@pAttribute
      if (length(pAttribute) == 0) stop("The partition is required to included counts. Enrich the object first!")
      coll <- new(
        "cooccurrences",
        pAttribute = pAttribute, corpus = .Object@corpus, encoding = .Object@encoding,
        left = window, right = window, partitionSize = .Object@size, stat = data.table()
      )
      coll@call <- deparse(match.call())
      coll@partition <- strsplit(deparse(sys.call(-1)), "\\(|\\)|,")[[1]][2]
      pAttr <- sapply(pAttribute, function(x) paste(.Object@corpus, ".", x, sep = ""))
      aColsId <- setNames(paste("a_", pAttribute, "_id", sep=""), pAttribute)
      bColsId <- setNames(paste("b_", pAttribute, "_id", sep=""), pAttribute)
      aColsStr <- setNames(paste("a_", pAttribute, sep=""), pAttribute)
      bColsStr <- setNames(paste("b_", pAttribute, sep=""), pAttribute)
      
      # turn tokens to keep to id 
      if (!is.null(keep)){
        if (all(pAttribute %in% names(keep))){
          keepId <- lapply(setNames(names(keep), names(keep)), function(x) CQI$str2id(.Object@corpus, x, keep[[x]]))  
        } else {
          stop("Count not performed for all pAttributes to keep.")
        }
      }
      
      if (big == TRUE){
        if (requireNamespace("bigmemory", quietly = TRUE) && requireNamespace("bigtabulate", quietly = TRUE) ) {
          if (verbose == TRUE) message("... generating context tables")
          BIG <- bigmemory::big.matrix(ncol = window * 2 + 1, nrow = .Object@size, ...)
          ids <- lapply(
            c(1:nrow(.Object@cpos)),
            function(i) 
              CQI$cpos2id(.Object@corpus, pAttribute, c(.Object@cpos[i,1]: .Object@cpos[i,2]))
          )
          idPos <- cumsum(lapply(ids, length))
          .windowPrep <- function(i, ids, window, BIG, ...){
            idChunk <- ids[[i]]
            lapply(
              c(-window:-1, 1:window),
              function(x){
                idsToFill <- c(
                  rep(NA, times = min(ifelse( x < 0 , -x, 0), length(idChunk))),
                  idChunk[
                    ifelse(length(idChunk) <= abs(x), 0, ifelse(x < 0, 1, x + 1))
                    :
                      ifelse(length(idChunk) <= abs(x), 0, ifelse(x < 0, length(idChunk)+x, length(idChunk)))
                    ],
                  rep(NA, times=min(ifelse(x > 0, x, 0), length(idChunk)))
                )
                BIG[c(ifelse(i == 1, 1, idPos[i-1]+1):idPos[i]), ifelse(x < 0, x + window + 1, x+window)] <- idsToFill
                BIG[c(ifelse(i == 1, 1, idPos[i-1]+1):idPos[i]), window * 2 + 1] <- idChunk
              })
          }
          dummy <- blapply(as.list(c(1:length(ids))), f = .windowPrep, ids = ids, window = window, BIG = BIG, mc = mc)
          if (verbose == TRUE) message("... counting cooccurrences")
          rowIndices <- bigtabulate::bigsplit(BIG, ccols = ncol(BIG), breaks=NA, splitcol=NA)
          .getTables <- function(node, rowIndices, BIG, window, ...){
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
          }
          tables <- blapply(
            as.list(names(rowIndices)), f=.getTables,
            rowIndices = rowIndices, BIG = BIG, window = window,
            mc = mc
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
        .makeWindows <- function(i, cpos, ...){
          cposMin <- cpos[i,1]
          cposMax <- cpos[i,2]
          if (cposMin != cposMax){
            cposRange <- cposMin:cposMax
            lapply(
              setNames(cposRange, cposRange),
              function(x) {
                cpos <- c((x - window):(x-1), (x + 1):(x + window))
                cpos <- cpos[which(cpos >= cposMin)]
                cpos[which(cpos <= cposMax)]
              })
          }
        }
        bag <- blapply(as.list(c(1:nrow(.Object@cpos))), f = .makeWindows, cpos = .Object@cpos, mc = mc)
        bCpos <- lapply(
          bag,
          function(x) lapply(names(x), function(y) rep(as.numeric(y), times = length(x[[y]])))
          )
        if (verbose) message("... putting together data.table")
        DT <- data.table(a_cpos = unlist(bag), b_cpos = unlist(bCpos))
        
        if (verbose == TRUE) message("... getting token ids")
        lapply(
          pAttribute, function(x){
            DT[, eval(aColsId[x]) := CQI$cpos2id(.Object@corpus, x, DT[["a_cpos"]]), with = TRUE]
            DT[, eval(bColsId[x]) := CQI$cpos2id(.Object@corpus, x, DT[["b_cpos"]]), with = TRUE]
          }
        )
        if (verbose == TRUE) message("... counting window size")
        # contextDT <- DT[, nrow(.SD), by = c(eval(aColsId)), with = TRUE] 
        contextDT <- DT[, .N, by = c(eval(aColsId)), with = TRUE] 
        setnames(contextDT, "N", "size_window")
        
        if (verbose == TRUE) message("... applying filter")
        if (!is.null(keep)){
          if (all(pAttribute %in% names(keep))){
            for (x in names(keep)){
              DT <- DT[DT[[aColsId[x]]] %in% keepId[[x]]]
              DT <- DT[DT[[bColsId[x]]] %in% keepId[[x]]]
            }
          }
        }
        
        if (verbose == TRUE) message("... counting co-occurrences")
        # TF <- DT[, nrow(.SD), by=c(eval(c(aColsId, bColsId))), with=TRUE] # not fast
        TF <- DT[, .N, by = c(eval(c(aColsId, bColsId))), with = TRUE]
        setnames(TF, "N", "count_ab")
        
        if (verbose == TRUE) message("... adding window size")
        setkeyv(contextDT, cols = aColsId)
        setkeyv(TF, cols = aColsId)
        TF <- contextDT[TF]
        
      }
      
      if (verbose == TRUE) message("... converting ids to strings")
      lapply(
        c(1:length(pAttribute)),
        function(i){
          TF[, eval(aColsStr[i]) := as.utf8(CQI$id2str(.Object@corpus, pAttribute[i], TF[[aColsId[i]]])), with = TRUE]
          TF[, eval(bColsStr[i]) := as.utf8(CQI$id2str(.Object@corpus, pAttribute[i], TF[[bColsId[i]]])), with=TRUE]
          TF[, eval(aColsId[i]) := NULL]
          TF[, eval(bColsId[i]) := NULL]
        }
      )
      setkeyv(TF, cols=aColsStr)
      setkeyv(.Object@stat, cols = pAttribute)
      TF[, "count_a" := .Object@stat[TF][["count"]]]
      setkeyv(TF, cols=bColsStr)
      TF[, "count_b" := .Object@stat[TF][["count"]]]
      setcolorder(TF, c(aColsStr, bColsStr, "count_ab", "count_a", "count_b", "size_window"))
      if (tcm == FALSE){
        coll@stat <- TF
        if ("ll" %in% method) {
          message('... g2-Test')
          coll <- ll(coll)
          coll@stat <- setorderv(coll@stat, cols="ll", order=-1)
        }
        return(coll)
      } else if (tcm == TRUE){
        concatenate <- function(x) paste(x, collapse = "//")
        if (length(pAttribute) > 1){
          TF[, "strKeyA" := apply(TF[, eval(paste("a", pAttribute, sep = "_")), with = FALSE], 1, concatenate)]
          TF[, "strKeyB" := apply(TF[, eval(paste("b", pAttribute, sep = "_")), with = FALSE], 1, concatenate)]
        } else {
          setnames(TF, old = paste("a", pAttribute, sep = "_"), new = "strKeyA")
          setnames(TF, old = paste("b", pAttribute, sep = "_"), new = "strKeyB")
        }
        uniqueKey <- unique(c(TF[["strKeyA"]], TF[["strKeyB"]]))
        keys <- setNames(c(1:length(uniqueKey)), uniqueKey)
        i <- unname(keys[TF[["strKeyA"]]])
        j <- unname(keys[TF[["strKeyB"]]])
        retval <- simple_triplet_matrix(
          i = i, j = j, v = TF[["count_ab"]],
          dimnames = list(a = names(keys)[1:max(i)], b = names(keys)[1:max(j)])
        )
        return(retval)
      } else {
        message("rcqp needs to be available")
      }
    }
  })

#' @rdname cooccurrences
setMethod("cooccurrences", "partitionBundle", function(.Object, mc=getOption("polmineR.mc"), ...){
  bundle <- new(
    "cooccurrencesBundle",
    encoding=unique(vapply(.Object@objects, function(x) x@encoding, FUN.VALUE="character")),
    corpus=unique(vapply(.Object@objects, function(x) x@corpus, FUN.VALUE="character"))
    )
  bundle@objects <- blapply(.Object@objects, f=cooccurrences, mc=mc, ...)
  names(bundle@objects) <- names(.Object@objects)
  bundle
})

