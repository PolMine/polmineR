setGeneric("as.svmlight", function(.Object, ...) standardGeneric("as.svmlight"))

setMethod("as.svmlight", "DocumentTermMatrix", function(.Object, filename){
  features <- split(x=.Object$j, f=.Object$i)
  values <- split(.Object$v, f=.Object$i)
  lapply(
    c(1:length(features)),
    function(i){
      featureOrder <- order(features[[i]])
      featureValues <- paste(
        features[[i]][featureOrder], values[[i]][featureOrder],
        sep=":"
        )
      cat(paste("1 ", paste(featureValues, collapse=" "), "\n", sep=""), file=filename, append=TRUE)
      return(TRUE)
    }
  )
  return(NULL)
})