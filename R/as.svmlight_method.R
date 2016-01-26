setGeneric("as.svmlight", function(.Object, ...) standardGeneric("as.svmlight"))

setMethod("as.svmlight", "DocumentTermMatrix", function(.Object, filename){
  features <- split(x=.Object@i, f=.Object@j)
  values <- split(.Object@v, f=.Object@j)
  lapply(
    c(1:length(features)),
    function(i){
      featureOrder <- order(features[[i]])
      featureValues <- paste(
        features[[i]][featureOrder], values[[i]][featureOrder],
        sep=":"
        )
      cat(paste0("1", featureValues, sep=" "), file=filename, append=TRUE)
      return(TRUE)
    }
  )
  return(NULL)
})