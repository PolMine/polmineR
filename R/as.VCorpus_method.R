#' Coerce partitionBundle to VCorpus.
#' 
#' @param x a partitionBundle object
#' @importFrom tm as.VCorpus
#' @exportMethod as.VCorpus
#' @rdname as.VCorpus
#' @examples
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' P <- partition("PLPRBTTXT", text_date = "2009-11-10")
#' PB <- partitionBundle(P, sAttribute = "text_speaker")
#' VC <- as.VCorpus(PB)
#' }
setMethod("as.VCorpus", "partitionBundle", function(x){
  content <- blapply(
    x@objects,
    function(P){
      metadata <- sapply(sAttributes(P), function(sAttr) sAttributes(P, sAttr)[1])
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