setGeneric("highlight", function(.Object, ...) standardGeneric("highlight"))

.makeTooltipTags <- function(color, tooltips){
  if (color %in% names(tooltips)) {
    return(list(
      start = paste('<a href="#" class="tooltip" data-tooltip="', tooltips[[color]], '">', sep=""),
      end = "</a>"
    ))
  } else {
    return(tooltipTags <- list(start="", end=""))
  }
}

setMethod("highlight", "partition", function(.Object, html, highlight=list(), tooltips=NULL){
  for (color in names(highlight)){
    tooltipTags <- .makeTooltipTags(color, tooltips)
    for (x in highlight[[color]]){
      hitCpos <- cpos(.Object, query=x)
      if (!is.null(hitCpos)){
        for (i in c(1:nrow(hitCpos))){
          for (j in c(hitCpos[i,1]:hitCpos[i,2])){
            html <- gsub(
              paste('<span id="', j, '">(.*?)</span>', sep=""),
              paste(
                paste('<span id="', j, '"><span style="background-color:', color, '">', sep=""),
                tooltipTags[["start"]], "\\1", tooltipTags[["end"]], '</span></span>', sep=""
              ),
              html
            )
          }
        }
      }
      
    }
  }
  html
})


setMethod("highlight", "character", function(.Object, highlight=list(), tooltips=NULL){
  for (color in names(highlight)){
    tooltipTags <- .makeTooltipTags(color, tooltips)
    for (x in highlight[[color]]){
      .Object <- gsub(
        paste("(", x, ")", sep=""),
        paste(
          paste('<span style="background-color:', color, '">', sep=""),
          tooltipTags[["start"]],
          "\\1", tooltipTags[["end"]], '</span>', sep=""
        ),
        .Object
      )
    }
  }
  .Object
})