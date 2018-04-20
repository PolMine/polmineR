#' Coerce partitionBundle to VCorpus.
#' 
#' @param x a partitionBundle object
#' @importFrom tm as.VCorpus
#' @exportMethod as.VCorpus
#' @rdname as.VCorpus
#' @examples
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = "2009-11-10")
#' PB <- partitionBundle(P, sAttribute = "speaker")
#' VC <- as.VCorpus(PB)
setMethod("as.VCorpus", "partitionBundle", function(x){
  sAttrLengths <- sapply(sAttributes(x@objects[[1]]@corpus), function(sAttr) CQI$attribute_size(x@objects[[1]]@corpus, sAttr, type = "s"))
  if (length(unique(sAttrLengths)) == length(sAttrLengths)){
    sAttrToGet <- sAttributes(x@objects[[1]]@corpus)
  } else {
    message("Using only the s-attributes that have the same length as the s-attribute in the slot sAttributeStrucs ",
            "of the first partition")
    sAttrToGet <- names(sAttrLengths[which(sAttrLengths == sAttrLengths[x@objects[[1]]@sAttributeStrucs])])
  }
  
  content <- blapply(
    x@objects,
    function(P){
      metadata <- sapply(sAttrToGet, function(sAttr) sAttributes(P, sAttr)[1])
      class(metadata) <- "TextDocumentMeta"
      doc <- list(
        meta = metadata,
        content = getTokenStream(P, pAttribute = "word", collapse = " ")
      )
      class(doc) <- c("PlainTextDocument", "TextDocument")
      doc
    }
  )
  names(content) <- names(x)
  toDrop <- which(sapply(content, function(c) nchar(c$content)) == 0)
  for (i in rev(toDrop)) content[[i]] <- NULL
  
  meta <- list()
  class(meta) <- "CorpusMeta"
  
  corpus <- list(content = content, meta = meta, dmeta = data.frame())
  class(corpus) <- c("VCorpus", "Corpus")
  corpus
})