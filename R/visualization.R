#' Line chart
#' 
#' Draw a line chart
#' 
#' Suitable for time series data. If time series data is provided, dates 
#' should be in columns 
#' 
#' @param tab a table to be presented
#' @param xlab xlab (cp. plot)
#' @param ylab ylab (cp. plot)
#' @param main main title (cp.plot)
#' @param rowsParties logical, either 'TRUE' or 'FALSE'
#' @param ymax maximum y value for chart
#' @return output is a plot
#' @author Andreas Blaette
#' @importFrom RColorBrewer brewer.pal
#' @export lineChart
lineChart <- function (tab, xlab="", ylab="", main="", rowsParties="TRUE", ymax=NA) {
  colors <- ifelse(
    rowsParties=="TRUE",
    unlist(sapply(rownames(tab), function(x) colorsParties[[x]])),
    append(brewer.pal(9,"Set1"), brewer.pal(12,"Set3"))[1:nrow(tab)]
    )
  ylim <- ifelse (
    is.na(ymax),
    ylim <- c(0, max(tab)),
    ylim <- c(0,ymax)
  )
  plot(c(0), c(0), xlab=xlab, ylab=ylab, main=main, xlim=c(1, ncol(tab)), xaxt="n", ylim=ylim, type="n")
  axis(1, at=c(1:ncol(tab)),labels=colnames(tab))
  for (i in 1:nrow(tab)) {
    lines(c(1:ncol(tab)), tab[i,], col=colors[i], lwd=3)
  }
  legend(c('top'), rownames(tab), cex=0.4, col=colors, lwd=5, ncol=2)
}

#' bubblegraph visualisation
#' 
#' visualise the distribution of counts/frequencies in a crosstab object with a bubblegraph
#' @param ctab a crosstabulation (a slot of a crosstab object, see example)
#' @param rex a radius expansion factor to control the bubble size
#' @param leftMargin adjust left margin of the plot
#' @param bottomMargin adjust bottom margin of the plot
#' @param cex character expansion factor
#' @param font set font parameter from par
#' @return a nice plot, hopefully
#' @examples
#' \dontrun{
#' bt <- partition(sAttributes=list(text_date="*"), method="grep", corpus="PLPRBTTXT", encoding="latin1")
#' dist <- dispersion(bt, "Politik", c("text_date", "text_party"))
#' bubblegraph(dist@@total$rel)
#' }
#' @export bubblegraph
bubblegraph <- function(ctab, rex=1, leftMargin=1.2, bottomMargin=1.6, cex=1, font=1) {
  par(mai=c(bottomMargin,leftMargin, 0.2, 0.2), mfcol=c(1,1), cex.axis=0.8)
  xrange <- c(1, ncol(ctab))
  yrange <- c(1, nrow(ctab))
  plot(c(1,1), type = "n", xlab=c(""), ylab=c(""),
       xlim=c(xrange[1]-0.5, xrange[2]+0.5),
       ylim=c(yrange[1]-0.5, yrange[2]+0.5),
       axes=F, main=c(""))
  box(lwd=2.0)
  par(cex=cex, font.axis=font)
  axis(side=2, labels=rownames(ctab), tick=TRUE, at=c(yrange[1]:yrange[2]), las=2)
  axis(side=1, labels=colnames(ctab), tick=TRUE, at=c(xrange[1]:xrange[2]), las=2)
  grid(col = "lightgray", lty = "dotted", lwd=1.5)  
  radius <- sqrt(ctab/pi)
  radius <- (radius/max(radius, na.rm=TRUE))/2 # Normalisierung der Radien
  radius <- radius * rex
  for ( i in 1:nrow(radius) ) { radius[i,] <- sapply(radius[i,], FUN = function(x){ if (is.nan(x)) {0} else {x}})}
  for (i in 1:(nrow(ctab))) {
    symbols(c(1:xrange[2]), rep(i, ncol(ctab)), circles=radius[i,], add=TRUE, bg="darkgrey", fg="darkgrey", inches=FALSE)
  }
}

#' word scatterplot
#' 
#' plot a word scatterplot
#' @param comp a data frame with the ranks of collocates
#' @param xmax maximum on x axis for plot
#' @param ymax maximum on y axis for plot
#' @param fontSize the expansion factor for the words
#' @param rotation rotation of the text
#' @param type defaults to 0 for output of terms, can be set to symbol integer 
#' @return the plot
#' @export scatterplotCollocates
#' @author Andreas Blaette
scatterplotCollocates <- function(comp, xmax=c(), ymax=c(), fontSize=0.7, rotation=45, type=0){
  plot(
    comp$stat[,"x.plot"],
    comp$stat[,"y.plot"],
    xlim=c(0, ifelse(is.null(xmax), c(max(comp$stat[,"x.plot"]), 0), xmax)),
    ylim=c(0, ifelse(is.null(ymax), c(max(comp$stat[,"y.plot"]), 0), ymax)),
    xlab=comp$partition.x,
    ylab=comp$partition.y,
    type="n"
  )
  par(bg="white")
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow2")
  grid(col="white")
  if (type==0) {
    text(comp$stat[,"x.plot"], comp$stat[,"y.plot"], rownames(comp$stat), cex=fontSize, srt=rotation)
  } else {
    points(comp$stat[,"x.plot"], comp$stat[,"y.plot"], pch=type)
  }
}

setMethod("plot", signature(x="partitionCluster", y="character"),
          function(x, y){
  val <- as.matrix(x, y)
  val <- val[rowSums(val)!=0,]
  data <- data.frame(rank(val[,1]), rank(val[,2]))
  plot(data[,1], data[,2])
})

#' barplot of a partitionCluster
#' 
#' @param pCluster a partitionCluster object
#' @exportMethod barplot
#' @noRd
setMethod("barplot", "partitionCluster", function(height, ...){
  tab <- summary(height)
  tab <- tab[order(tab[, "token"], decreasing=TRUE),]
  barplot(tab$token, names.arg=tab$partition, ...)
})