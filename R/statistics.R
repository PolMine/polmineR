#' filter a context object
#' 
#' Just make it more handy and nicer
#' 
#' Maybe it would be more efficient to use the subset function.-
#' 
#' @param object a context object to be filtered
#' @param min.significance minimum significance level
#' @param min.frequency the minimum frequency
#' @param max.rank maximum rank
#' @param pos.filter exclude words with a POS tag not in this list
#' @author Andreas Blaette
#' @noRd
.consolidate <- function(object, min.significance=0, min.frequency=0, max.rank=0, pos.filter=c()){
  if (max.rank==0) max.rank=dim(object@stat)[1]
  object@stat <- object@stat[order(object@stat[,4], decreasing=TRUE),]
  object@stat <- object@stat[which(object@stat[,4]>=min.significance),]
  object@stat <- object@stat[which(object@stat[,"obs.coi"]>=min.frequency),]
  if (length(pos.filter)!=0) {
    cat('... adding part-of-speech tags to statistics-table (may take a while)\n')
    object <- .add.pos(object)
    object@stat<- object@stat[which(object@stat[,"pos"] %in% pos.filter),]
  }
  object@stat[,"rank"] <- c(1:length(object@stat[,"rank"]))
  object@stat <- object@stat[which(object@stat[,"rank"]<=max.rank),]
  object
}





#' Add POS tags
#' 
#' Add the POS tags to a table with tokens in the rows
#' 
#' The POS tags that occur for a given token are counted. The POS tag with the
#' highest share is added to the table
#' 
#' @param object a context object
#' @return An amended table
#' @author Andreas Blaette
#' @noRd
.add.pos <- function(object) {
  pos.col <- c()
  share.col <- c()
  ids = cqi_str2id(paste(object@corpus, ".", object@pattribute, sep=""), rownames(object@stat))
  for (i in 1:dim(object@stat)[1]){
    pos = count(cqi_cpos2str(paste(object@corpus, ".pos", sep=""),cqi_id2cpos(paste(object@corpus, ".", object@pattribute, sep=""), ids[i])))
    pos <- pos[order(pos[,2],decreasing=TRUE),]
    pos.col <- c(pos.col, as.character(pos[1,1]))
    share.col <- c(share.col, round(pos[1,2]/sum(pos[,2])*100, digits=1))
  }
  object@stat <- cbind(object@stat, pos=pos.col, pos.share=share.col)
  object
}
  

#' Add POS tags
#' 
#' Add the POS tags to a table with tokens in the rows
#' 
#' The POS tags that occur for a given token are counted. The POS tag with the
#' highest share is added to the table
#' 
#' @param object a context object
#' @return An amended table
#' @author Andreas Blaette
#' @noRd
.addPos <- function(object) {
  ids = cqi_str2id(paste(object@corpus, ".", object@pattribute, sep=""), rownames(object@stat))
  posIds <- unlist(mclapply(ids, function (x){
    idPos <- cqi_cpos2id(paste(object@corpus, ".pos", sep=""), cqi_id2cpos(paste(object@corpus, ".", object@pattribute, sep=""), x))
    posIdFrequencies <- tabulate(idPos+1)
    mostFrequent <- which.max(posIdFrequencies) - 1
    return(mostFrequent)
  }))
  pos <- cqi_id2str(paste(object@corpus, ".pos", sep=""), posIds)
  object@stat <- cbind(object@stat, pos=pos)
  object
}

#' S4 class for comparing corpora
#' 
#' to keep results from a keyness analysis
#' 
#' @section Objects from the class:
#' keyness objects are returned by the function call \code{keyness}
#'   
#' @section Slots:
#' \describe{ \item{\code{corpus}:}{Object of class
#'   \code{"character"} ~~ } \item{\code{pattribute}:}{Object of class
#'   \code{"character"} ~~ } \item{\code{stat}:}{Object of class
#'   \code{"data.frame"} ~~ } }
#' @rdname keyness-class
#' @name keyness-class
#' @docType class
#' @exportClass
#' @author Andreas Blaette
setClass("keyness",
         representation(corpus="character",
                        pattribute="character",
                        stat="data.frame"
         )
)





#' compute chi-square values for tokens in a corpus using a reference corpus
#' 
#' Pearson's chi-squared test is calculated to measure the keyness of a token
#' in a corpus of interest (COI). A reference corpus is required for the
#' computation
#' 
#' If pos.filter is supplied, the most propable pos-Tags will be added to the 
#' statistical table and the table will be filtered. This may slow down the 
#' procedure considerably.
#' 
#' 
#' @param coi a partition object
#' @param ref a partition object, it is assumed that the coi is a subcorpus of
#' ref
#' @param pattribute The P-Attribute that will be counted (usually either
#' 'word' or 'lemma')
#' @param included TRUE if coi is part of ref, defaults to FALSE
#' @param min.significance the minimum statistical test value
#' @param min.frequency the minimum frequency of a token
#' @param verbose defaults to TRUE
#' @param pos.filter a character vector 
#' @return The function returns a data frame with the following structure:
#' - absolute frequencies in the first row
#' - ...
#' @author Andreas Blaette
#' @references Manning / Schuetze ...
#' @export keyness
keyness <- function(
  coi,
  ref,
  pattribute=drillingControls$pAttribute,
  included=FALSE,
  min.significance=drillingControls$minSignificance,
  min.frequency=drillingControls$minFrequency,
  verbose=TRUE,
  pos.filter=NULL
  ) {
  if (verbose==TRUE) message ('Computing keyness')
  keyness <- new('keyness')
  if (verbose==TRUE) message("... combining frequency lists")
  c <- merge(coi@tf[[pattribute]], ref@tf[[pattribute]], by.x="id", by.y="id")
  if (verbose==TRUE) message("... computing chisquare tests")
  c <- .chisquare(c, included, min.frequency)
  statistic <- data.frame(
    row.names=cqi_id2str(paste(coi@corpus,".", pattribute, sep=""), c[,1]),
    rank=c(1:dim(c)[1]),
    obs.coi=c[,2],
    obs.ref=c[,3],
    chi.square=round(c[,4], digits=2),
    exp.coi=round(c[,5], digits=2),
    exp.ref=round(c[,6], digits=2)
  )
  Encoding(rownames(statistic)) <- ref@encoding
  keyness@corpus <- coi@corpus
  keyness@pattribute <- pattribute
  keyness@stat <- statistic
  if (verbose==TRUE) message("... filtering results")
  keyness <- .consolidate(keyness, min.significance, min.frequency, pos.filter=pos.filter)  
  keyness
}


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
  calc <- cbind(nodeId, .calc.g2(collocateId, collocateWindowFreq, windowSize, partitionObject, pAttribute))
  tab <- data.frame(node=cqi_id2str("PLPRBTTXT.word", calc[,1]),
                       collocate=cqi_id2str("PLPRBTTXT.word", calc[,2]),
                       calc)
  tab[,1] <- as.character(tab[,1])
  tab[,2] <- as.character(tab[,2])
  Encoding(tab[,1]) <- partitionObject@encoding
  Encoding(tab[,2]) <- partitionObject@encoding
  tab <- tab[order(tab[,9], decreasing=T),]
  tab
}


.calc.g2 <- function(windowIds, windowFreq, windows.total, partitionObject, pAttribute){
  calc <- matrix(data=0, nrow=length(windowFreq), ncol=6)
  colnames(calc) <- c("collocateId", "freqObsWindow", "freqObsCorpus", "freqExpWindow", "freqExpCorpus", "g2-Test")
  calc[,1] <- windowIds
  calc[,2] <- windowFreq
  calc[,3] <- partitionObject@tf[[pAttribute]][match(calc[,1], partitionObject@tf[[pAttribute]][,1]),2]
  calc[,4] <- windows.total*calc[,3]/partitionObject@size
  calc[,5] <- (partitionObject@size-windows.total)*calc[,3]/partitionObject@size
  calc[,6] <- 2*(calc[,2]*log(calc[,2]/calc[,4])+((calc[,3]-calc[,4])*log((calc[,3]-calc[,4])/calc[,5])))
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
