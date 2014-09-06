#' @include partition-class.R
NULL

setGeneric("bubblegraph", function(object,...){standardGeneric("bubblegraph")})

#' bubblegraph visualisation
#' 
#' visualise the distribution of counts/frequencies in a crosstab object with a bubblegraph
#' 
#' @param object a crosstabulation (a slot of a crosstab object, see example)
#' @param rex a radius expansion factor to control the bubble size
#' @param leftMargin adjust left margin of the plot
#' @param bottomMargin adjust bottom margin of the plot
#' @param cex character expansion factor
#' @param font set font parameter from par
#' @return a nice plot, hopefully
#' @exportMethod bubblegraph
#' @examples
#' \dontrun{
#' bt <- partition(sAttributes=list(text_date="*"), method="grep", corpus="PLPRBTTXT", encoding="latin1")
#' dist <- dispersion(bt, "Politik", c("text_date", "text_party"))
#' bubblegraph(dist@@total$rel)
#' }
#' @docType methods
#' @rdname bubblegraph-method.Rd
#' @name bubblegraph
#' @aliases bubblegraph bubblegraph-method bubblegraph,data.frame-method
setMethod("bubblegraph", "data.frame", function(object, rex=1, leftMargin=1.2, bottomMargin=1.6, cex=1, font=1) {
  par(mai=c(bottomMargin,leftMargin, 0.2, 0.2), mfcol=c(1,1), cex.axis=0.8)
  xrange <- c(1, ncol(object))
  yrange <- c(1, nrow(object))
  plot(c(1,1), type = "n", xlab=c(""), ylab=c(""),
       xlim=c(xrange[1]-0.5, xrange[2]+0.5),
       ylim=c(yrange[1]-0.5, yrange[2]+0.5),
       axes=F, main=c(""))
  box(lwd=2.0)
  par(cex=cex, font.axis=font)
  axis(side=2, labels=rownames(object), tick=TRUE, at=c(yrange[1]:yrange[2]), las=2)
  axis(side=1, labels=colnames(object), tick=TRUE, at=c(xrange[1]:xrange[2]), las=2)
  grid(col = "lightgray", lty = "dotted", lwd=1.5)  
  radius <- sqrt(object/pi)
  radius <- (radius/max(radius, na.rm=TRUE))/2 # Normalisierung der Radien
  radius <- radius * rex
  for ( i in 1:nrow(radius) ) { radius[i,] <- sapply(radius[i,], FUN = function(x){ if (is.nan(x)) {0} else {x}})}
  for (i in 1:(nrow(object))) {
    symbols(c(1:xrange[2]), rep(i, ncol(object)), circles=radius[i,], add=TRUE, bg="darkgrey", fg="darkgrey", inches=FALSE)
  }
})
