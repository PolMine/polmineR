#' Obtain frequencies
#' 
#' Get term frequencies for a partition object. This is a helper function
#' for \code{partition}.
#'
#' @param part a partition object
#' @param pAttribute either 'word' or 'lemma'
#' @noRd
.cpos2tf <- function(part, pAttribute){
  cpos <- unlist(apply(part@cpos, 1, function(x) x[1]:x[2]))
  ids <- cqi_cpos2id(paste(part@corpus, '.', pAttribute, sep=''), cpos)
  tfRaw <- tabulate(ids)
  tf <- data.frame(
    id=c(0:length(tfRaw)),
    tf=c(length(ids[which(ids==0)]), tfRaw),
    row.names=cqi_id2str(paste(part@corpus,'.',pAttribute, sep=''), c(0:length(tfRaw)))
  )
  Encoding(rownames(tf)) <- part@encoding
  tf <- subset(tf, tf > 0)
  tf
}

