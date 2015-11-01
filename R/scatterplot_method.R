#' @include partition_class.R
NULL

setGeneric("scatterplot", function(object,...){standardGeneric("scatterplot")})


#' word scatterplot
#' 
#' plot a word scatterplot
#' @param object a data frame with the ranks of cooccurrences
#' @param xmax maximum on x axis for plot
#' @param ymax maximum on y axis for plot
#' @param fontSize the expansion factor for the words
#' @param rotation rotation of the text
#' @param type defaults to 0 for output of terms, can be set to symbol integer 
#' @return the plot
#' @exportMethod scatterplot
#' @docType methods
#' @author Andreas Blaette
#' @name scatterplot
#' @rdname scatterplot-method
#' @aliases scatterplot scatterplot-method scatterplot,data.frame-method
setMethod("scatterplot", "data.frame", function(object, xmax=c(), ymax=c(), fontSize=0.7, rotation=45, type=0){
  plot(
    object$stat[,"x.plot"],
    object$stat[,"y.plot"],
    xlim=c(0, ifelse(is.null(xmax), c(max(object$stat[,"x.plot"]), 0), xmax)),
    ylim=c(0, ifelse(is.null(ymax), c(max(object$stat[,"y.plot"]), 0), ymax)),
    xlab=object$partition.x,
    ylab=object$partition.y,
    type="n"
  )
  par(bg="white")
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow2")
  grid(col="white")
  if (type==0) {
    text(object$stat[,"x.plot"], object$stat[,"y.plot"], rownames(object$stat), cex=fontSize, srt=rotation)
  } else {
    points(object$stat[,"x.plot"], object$stat[,"y.plot"], pch=type)
  }
})

