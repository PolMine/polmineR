
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

