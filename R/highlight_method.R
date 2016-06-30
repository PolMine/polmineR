#' highlight tokens based on cpos or regex
#' 
#' The method is a worker for the read method and is not exported.
#' 
#' @param .Object either a \code{"partition"} or \code{"character"} object
#' @param html character vector with a website
#' @param highlight a \code{"list"} of character vectors, the names need to provide the colors
#' @param tooltips a \code{"list"} of character vectors, all names need to be included in lists of the highlight-list
#' @noRd
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

setMethod("highlight", "html", function(.Object, highlight=list(), tooltips=NULL){
  for (color in names(highlight)){
    tooltipTags <- .makeTooltipTags(color, tooltips)
    for (i in highlight[[color]]){
      if (is.numeric(i)){
        .Object <- gsub(
          paste('<span id="', i, '">(.*?)</span>', sep=""),
          paste(
            paste('<span id="', i, '"><span style="background-color:', color, '">', sep=""),
            tooltipTags[["start"]], "\\1", tooltipTags[["end"]], '</span></span>', sep=""
          ),
          .Object
        )
      } else {
        .Object <- gsub(
          paste("(", i, ")", sep=""),
          paste(
            paste('<span style="background-color:', color, '">', sep=""),
            tooltipTags[["start"]],
            "\\1", tooltipTags[["end"]], '</span>', sep=""
          ),
          .Object
        )
        
      }
    }
    
  }
  .Object
})

setMethod("highlight", "partition", function(.Object, html, highlight=list(), cqp=is.cqp, tooltips=NULL){
  for (color in names(highlight)){
    tooltipTags <- .makeTooltipTags(color, tooltips)
    for (x in highlight[[color]]){
      hitCpos <- cpos(.Object, query=x, cqp=cqp)
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