#' @include textstat-class.R keyness-class.R context-class.R
NULL

setGeneric("chisquare", function(x, ...){standardGeneric("chisquare")})

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
#' @param minFrequency minimum frequency for a token to be kept in matrix
#' @return a table
#' @author Andreas Blaette
#' @noRd
.chisquare <- function(ctab, sizeCoi, sizeRef, minFrequency) {
  ctab <- ctab[which(ctab[,"countCoi"] > minFrequency),]
  o <- matrix(data=0, nrow=nrow(ctab), ncol=6)
  o[,1] <- ctab[,2]
  o[,2] <- ctab[,3]
  o[,3] <- o[,1] + o[,2]
  o[,4] <- sizeCoi - o[,1]
  o[,5] <- sizeRef - o[,2]
  o[,6] <- o[,4] + o[,5]
  sizeTotal <- sizeCoi + sizeRef
  e <- matrix(data=0, nrow=dim(o)[1], ncol=4)
  options(digits=20)
  e[,1] <- (o[,3]/sizeTotal) * sizeCoi
  e[,2] <- (o[,3]/sizeTotal) * sizeRef
  e[,3] <- (o[,6]/sizeTotal) * sizeCoi
  e[,4] <- (o[,6]/sizeTotal) * sizeRef
  chi <- ((e[,1]-o[,1])**2)/e[,1]+((e[,2]-o[,2])**2)/e[,2]+((e[,3]-o[,4])**2)/e[,3]+((e[,4]-o[,5])**2)/e[,4]
  chi <- chi*apply(cbind(o[,1], e[,1]), MARGIN=1, function(x){if (x[1]>x[2]){1} else {-1}})
  options(digits=7)
  ctab$expCoi <- e[,1]
  ctab$expRef <- e[,2]
  ctab$chiSquare <- chi
  # retval <- cbind(ctab, expCoi=e[,1], expRef=e[,2], chiSquare=chi)
  return(ctab)
}

setMethod("chisquare", "keyness", function(x){
  x@stat <- .chisquare(
    ctab=x@stat,
    sizeCoi=x@sizeCoi,
    sizeRef=x@sizeRef,
    minFrequency=x@minFrequency
    )
  x@statisticalTest <- c(x@statisticalTest, "chiSquare")
  return(x)
})
