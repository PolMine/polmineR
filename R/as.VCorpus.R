#' @include S4classes.R
NULL

setOldClass("VCorpus")

#' Coerce partition_bundle to VCorpus.
#' 
#' Retrieve full text for the \code{partition} objects in a
#' \code{partition_bundle} and generate a \code{VCorpus}-class object from the
#' \code{tm}-package.
#' 
#' The \code{VCorpus} class of the \code{tm}-package offers an interface to
#' access the functionality of the \code{tm}-package. Note however that
#' generating a \code{VCorpus} to get a \code{DocumentTermMatrix}, or a
#' \code{TermDocumentMatrix} is a highly inefficient detour. Applying the
#' \code{as.DocumentTermMatrix} or \code{as.TermDocumentMatrix} methods on a
#' \code{partition_bundle} is the recommended approach.
#' 
#' If the \code{tm}-package has been loaded, the \code{as.VCorpus}-method
#' included in the \code{polmineR}-package may become inaccessible. To deal with
#' this (propable) scenario, it is possible to use a coerce-method
#' (\code{as(YOUROBJECT, "VCorpus")}), see examples.
#' 
#' @param x A \code{partition_bundle} object.
#' @importFrom tm as.VCorpus
#' @exportMethod as.VCorpus
#' @rdname as.VCorpus
#' @name as.VCorpus
#' @aliases as.VCorpus,partition_bundle-method
#' @examples
#' use("polmineR")
#' p <- partition("GERMAPARLMINI", date = "2009-11-10")
#' pb <- partition_bundle(p, s_attribute = "speaker")
#' vc <- as.VCorpus(pb) # works only, if tm-package has not yet been loaded
#' vc <- as(pb, "VCorpus") # will work if tm-package has been loaded, too
setMethod("as.VCorpus", "partition_bundle", function(x){
  as(x, "VCorpus")
})

#' @name as
#' @rdname as.VCorpus
setAs(from = "partition_bundle", to = "VCorpus", def = function(from){
  s_attr_lengths <- sapply(
    s_attributes(from@objects[[1]]@corpus),
    function(s_attr) CQI$attribute_size(from@objects[[1]]@corpus, s_attr, type = "s")
  )
  
  if (length(unique(s_attr_lengths)) == length(s_attr_lengths)){
    s_attr_to_get <- s_attributes(from@objects[[1]]@corpus)
  } else {
    message("Using only the s-attributes that have the same length as the s-attribute in the slot s_attribute_strucs ",
            "of the first partition")
    s_attr_to_get <- names(s_attr_lengths[which(s_attr_lengths == s_attr_lengths[from@objects[[1]]@s_attribute_strucs])])
  }
  
  content <- blapply(
    from@objects,
    function(P){
      metadata <- sapply(s_attr_to_get, function(s_attr) s_attributes(P, s_attr)[1])
      class(metadata) <- "TextDocumentMeta"
      doc <- list(
        meta = metadata,
        content = get_token_stream(P, p_attribute = "word", collapse = " ")
      )
      class(doc) <- c("PlainTextDocument", "TextDocument")
      doc
    }
  )
  names(content) <- names(from)
  to_drop <- which(sapply(content, function(c) nchar(c$content)) == 0)
  for (i in rev(to_drop)) content[[i]] <- NULL
  
  meta <- list()
  class(meta) <- "CorpusMeta"
  
  y <- list(content = content, meta = meta, dmeta = data.frame())
  class(y) <- c("VCorpus", "Corpus")
  y
})