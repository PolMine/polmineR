setGeneric("highlight", function(.Object, ...) standardGeneric("highlight"))

setMethod("highlight", "partition", function(.Object, html, highlight=list()){
  for (color in names(highlight)){
    for (x in highlight[[color]]){
      hitCpos <- cpos(.Object, query=x)
      for (i in c(1:nrow(hitCpos))){
        for (j in c(hitCpos[i,1]:hitCpos[i,2])){
          html <- gsub(
            paste('<span id="', j, '">(.*?)</span>', sep=""),
            paste(
              paste('<span id="', j, '"><span style="background-color:', color, '">', sep=""),
              "\\1", '</span></span>', sep=""
            ),
            html
          )
        }
      }
    }
  }
  html
})


setMethod("highlight", "character", function(.Object, highlight=list()){
  for (color in names(highlight)){
    for (x in highlight[[color]]){
      .Object <- gsub(
        paste("(", x, ")", sep=""),
        paste(
          paste('<span style="background-color:', color, '">', sep=""),
          "\\1", '</span>', sep=""
        ),
        .Object
      )
    }
  }
  .Object
})