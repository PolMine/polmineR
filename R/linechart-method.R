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
#' @export linechart
linechart <- function (tab, xlab="", ylab="", main="", rowsParties="TRUE", ymax=NA) {
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


