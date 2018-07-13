#' @include S4classes.R
NULL

#' Coerce partition_bundle to VCorpus.
#' 
#' @param x a \code{partition_bundle} object
#' @importFrom tm as.VCorpus
#' @exportMethod as.VCorpus
#' @rdname as.VCorpus
#' @name as.VCorpus
#' @aliases as.VCorpus,partition_bundle-method
#' @examples
#' use("polmineR")
#' P <- partition("GERMAPARLMINI", date = "2009-11-10")
#' PB <- partition_bundle(P, s_attribute = "speaker")
#' VC <- as.VCorpus(PB)
setMethod("as.VCorpus", "partition_bundle", function(x){
  sAttrLengths <- sapply(s_attributes(x@objects[[1]]@corpus), function(sAttr) CQI$attribute_size(x@objects[[1]]@corpus, sAttr, type = "s"))
  if (length(unique(sAttrLengths)) == length(sAttrLengths)){
    sAttrToGet <- s_attributes(x@objects[[1]]@corpus)
  } else {
    message("Using only the s-attributes that have the same length as the s-attribute in the slot s_attribute_strucs ",
            "of the first partition")
    sAttrToGet <- names(sAttrLengths[which(sAttrLengths == sAttrLengths[x@objects[[1]]@s_attribute_strucs])])
  }
  
  content <- blapply(
    x@objects,
    function(P){
      metadata <- sapply(sAttrToGet, function(sAttr) s_attributes(P, sAttr)[1])
      class(metadata) <- "TextDocumentMeta"
      doc <- list(
        meta = metadata,
        content = get_token_stream(P, p_attribute = "word", collapse = " ")
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