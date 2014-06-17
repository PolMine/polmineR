
#' perform chisquare-text
#' 
#' Perform Chisquare-Test based on a table with counts
#' 
#' This function deliberately uses a self-made chi-square test for performance
#' reason
#' 
#' @param ctab a matrix with ids in column 1 term frequencies in col 2, and
#' overall frequencies in col 3
#' @param included defaults to FALSE, YES if corpus of interest is included in
#' reference corpus
#' @param min.frequency minimum frequency for a token to be kept in matrix
#' @return a table
#' @author Andreas Blaette
#' @noRd
.chisquare <- function(ctab, included=FALSE, min.frequency) {
  ctab <- ctab[which(ctab[,1]>min.frequency),]
  size.coi <- sum(ctab[,2])
  size.ref <- sum(ctab[,3])
  if (included == TRUE) {
    ctab[,3] <- ctab[,3]-ctab[,2]
    size.ref <- size.ref-size.coi
  }
  o <- matrix(data=0, nrow=dim(ctab)[1], ncol=6)
  o[,1] <- ctab[,2]
  o[,2] <- ctab[,3]
  o[,3] <- o[,1]+o[,2]
  o[,4] <- size.coi-o[,1]
  o[,5] <- size.ref-o[,2]
  o[,6] <- o[,4]+o[,5]
  size.total <- size.coi+size.ref
  e <- matrix(data=0, nrow=dim(o)[1], ncol=4)
  options(digits=20)
  e[,1] <- (o[,3]/size.total)*size.coi
  e[,2] <- (o[,3]/size.total)*size.ref
  e[,3] <- (o[,6]/size.total)*size.coi
  e[,4] <- (o[,6]/size.total)*size.ref
  chi <- ((e[,1]-o[,1])**2)/e[,1]+((e[,2]-o[,2])**2)/e[,2]+((e[,3]-o[,4])**2)/e[,3]+((e[,4]-o[,5])**2)/e[,4]
  chi <- chi*apply(cbind(o[,1], e[,1]), MARGIN=1, function(x){if (x[1]>x[2]){1} else {-1}})
  options(digits=7)
  return <- cbind(ctab, chi=chi, exp.coi=e[,1], exp.ref=e[,2])
}







#' calculate all collocations in a partition
#' 
#' the result is meant to serve as a result for an analysis of collocation graphs
#' 
#' @param partitionObject a partition object
#' @param pAttribute p-attribute, typically "word" or "token"
#' @param window no of tokens to the left and to the right
#' @param filter defaults to TRUE
#' @param posFilter what POS to keep
#' @param multicore whether to use multicore
#' @return a data frame
#' @author Andreas Blaette
#' @export collocations
collocations <- function(partitionObject, pAttribute="word", window=5, filter=TRUE, posFilter=c("ADJA", "NN"), multicore=FALSE){
  tokenAttr <- paste(partitionObject@corpus,".",pAttribute, sep="")
  posAttr <- paste(partitionObject@corpus,".pos", sep="")
  getIdsWindow <- function(x, window, cposMax, ids, pos){
    j <- c((x-window):(x-1), (x+1):(x+window))
    j <- j[which(j > 0)]
    j <- j[which(j <= cposMax)]
    id <- ids[j]
    names(id) <- pos[j]
    id
  }
  getNodeIds <- function(x, neighbours, ids, pos) {
    v <- rep(ids[x], times=length(neighbours[[x]]))
    names(v) <- rep(pos[x], times=length(neighbours[[x]]))
    v
  } 
  movingContext <- function (cposRow, window, partitionObject, tokenAttr, posAttr) {
    bag <- list()
    cposRange <- c(partitionObject@cpos[cposRow,1]:partitionObject@cpos[cposRow,2])
    ids <- cqi_cpos2id(tokenAttr, cposRange)
    pos <- cqi_cpos2id(posAttr,cposRange)
    neighbours <- lapply(c(1:length(cposRange)), function(x) getIdsWindow(x,window,length(cposRange), ids, pos))
    bag[['nodes']] <- unlist(lapply(c(1:length(cposRange)),
                                              function(x) getNodeIds(x, neighbours, ids, pos)))
    bag[['neighbourhood']] <- unlist(neighbours)
    bag
  }
  message('... creating window lists')
  if (multicore==FALSE){
    bag <- lapply(c(1:nrow(partitionObject@cpos)), function(cposRow) {b <- movingContext(cposRow, window, partitionObject, tokenAttr, posAttr)})
  } else {
    bag <- mclapply(c(1:nrow(partitionObject@cpos)), function(cposRow) {b <- movingContext(cposRow, window, partitionObject, tokenAttr, posAttr)})
  }
  nodes <- lapply(bag, function(x) x$nodes)
  neighbourhood <- lapply(bag, function(x) x$neighbourhood)
  idFrame <- data.frame(
    nodeId=unname(unlist(nodes)),
    nodePos=as.integer(names(unlist(nodes))),
    tokenId=unname(unlist(neighbourhood)),
    podId=as.integer(names(unlist(neighbourhood)))
    )
  idFrameSelect <- idFrame[which(idFrame[,2] %in% cqi_str2id(posAttr, posFilter)),]
  idFrameSelect <- idFrameSelect[which(idFrameSelect[,4] %in% cqi_str2id(posAttr, posFilter)),]
  message('... pre-sorting for frequency count')
  frameSplit <- split(idFrameSelect[,1], idFrameSelect[,3])
  message('... now for the actual frequency count')
  if (multicore==FALSE){
    raw <- lapply(frameSplit, table)
  } else {
    raw <- mclapply(frameSplit, table)
  }
  message('... re-arrange data')
  nodeId <- unlist(lapply(names(raw), function(x) rep(as.numeric(x), times=length(raw[[x]]))))
  collocateId <- unlist(lapply(raw, function(x) as.numeric(names(x))))
  collocateWindowFreq <- unlist(lapply(raw, function(x) unname(x)))
  windowSize <- unlist(lapply(raw, function(x) rep(sum(x), times=length(x))))
  message('... g2-Test')
  calc <- cbind(nodeId, .g2Statistic(collocateId, collocateWindowFreq, windowSize, partitionObject, pAttribute))
  tab <- data.frame(node=cqi_id2str(tokenAttr, calc[,1]),
                       collocate=cqi_id2str(tokenAttr, calc[,2]),
                       calc)
  tab[,1] <- as.character(tab[,1])
  tab[,2] <- as.character(tab[,2])
  Encoding(tab[,1]) <- partitionObject@encoding
  Encoding(tab[,2]) <- partitionObject@encoding
  tab <- tab[order(tab[,9], decreasing=T),]
  tab
}


.g2Statistic <- function(windowIds, windowFreq, windows.total, partitionObject, pAttribute){
  calc <- matrix(data=0, nrow=length(windowFreq), ncol=6)
  colnames(calc) <- c("collocateId", "countCoi", "countCorpus", "expCoi", "expCorpus", "LL")
  calc[,1] <- windowIds
  calc[,2] <- windowFreq
  calc[,3] <- partitionObject@tf[[pAttribute]][match(calc[,1], partitionObject@tf[[pAttribute]][,1]),2]
  calc[,4] <- windows.total*calc[,3]/partitionObject@size
  calc[,5] <- (partitionObject@size-windows.total)*calc[,3]/partitionObject@size
  calc[,6] <- 2*(calc[,2]*log(calc[,2]/calc[,4])+((calc[,3]-calc[,4])*log((calc[,3]-calc[,4])/calc[,5])))
  calc
}

#' Calculate pointwise mutual information (PMI)
#' 
#' There may be a (small) problem: the counts used are not cooccurrences ...
#' 
#' @param windowIds ids of cooccurring words, used for matching
#' @param windowFreq count for occurrence of collocates
#' @param countTarget counts of targe in partition (total) 
#' @param partitionObject here we get the counts for cooccurring tokens
#' @param pattribute needed for matching
#' @noRd
.pmi <- function(windowIds, windowFreq, countTarget, partitionObject, pAttribute){
  calc <- matrix(data=0, nrow=length(windowFreq), ncol=4)
  size <- partitionObject@size
  colnames(calc) <- c("collocateId", "countCoi", "countPartition", "pmi")
  calc[,1] <- windowIds
  calc[,2] <- windowFreq
  calc[,3] <- partitionObject@tf[[pAttribute]][match(calc[,1], partitionObject@tf[[pAttribute]][,1]),2]
  calc[,4] <- log2((calc[,2]/size)/((countTarget/size)*(calc[,3]/size)))
  calc
}

# 
# library(inline)
# src <- '
# Rcpp::IntegerVector xa(a);
# int n_xa = xa.size();
# int test = max(xa);
# Rcpp::IntegerVector xab(test);
# for (int i = 0; i < n_xa; i++)
# xab[xa[i]-1]++;
# return xab;
# '
# fun <- cxxfunction(signature(a = "integer"),src, plugin = "Rcpp")

