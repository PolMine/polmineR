run <- function(...){
  passedArgs <- request[["query.string"]]
  passedArgs <- URLdecode(passedArgs)
  sAttrRaw <- lapply(unlist(strsplit(passedArgs[1], "__")), function(x){unlist(strsplit(x, "="))})
  names(sAttrRaw) <- lapply(sAttrRaw, function(x)x[1])
  sAttr <- lapply(sAttrRaw, function(x) gsub("\\+", " ", x[2]))
  if ("corpus" %in% names(sAttr)){
    corpus <- sAttr[["corpus"]]
    sAttr[["corpus"]] <- NULL
    partitionToDisplay <- partition(object=corpus, def=sAttr, pAttribute=NULL, verbose=FALSE)
    partitionAsHtml <- html(partitionToDisplay)
#   out("Content-type: text/html\n\n")
    out(partitionAsHtml)
  }
}
