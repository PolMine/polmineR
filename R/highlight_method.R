setGeneric("highlight", function(.Object, ...) standardGeneric("highlight"))

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