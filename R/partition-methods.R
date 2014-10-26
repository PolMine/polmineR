#' @include partition-class.R
NULL

#' @exportMethod show
#' @docType methods
#' @noRd
setMethod("show", "partition",
function(object){
  cat("** partition object **\n")
  cat(sprintf("%-20s", "CWB-corpus:"), object@corpus, "\n")
  cat(sprintf("%-20s", "Label:"), object@label, "\n")
  if (length(object@sAttributes)==0) {
    cat(sprintf("%-20s", "S-Attributes:"), "no specification\n")
  } else {
    s <- unlist(lapply(
      names(object@sAttributes),
      function(x) {paste(x, "=", paste(object@sAttributes[[x]], collapse="/"))}
      ))
    cat(sprintf("%-20s", "S-attributes:"), s[1], '\n')
    if (length(s)>1) {for (i in length(s)){cat(sprintf("%-20s", " "), s[i], '\n')}}
  } 
  cat(sprintf("%-21s", "Corpus positions:"))
  if (nrow(object@cpos)==0) {cat("not available\n")}
  else {cat(nrow(object@cpos), "pairs of corpus positions\n")}
  cat(sprintf("%-21s", "Partition size:"))
  if (is.null(object@size)) {cat("not available\n")}
  else {cat(object@size, "tokens\n")}
  cat(sprintf("%-21s", "Term frequencies:"))
  if (length(object@tf)==0) {cat("not available\n")}
  else {cat("available for", paste(names(object@tf), collapse=", "), "\n")}
})




#' @exportMethod [
#' @docType methods
#' @noRd
setMethod('[', 'partition', function(x,i){
  hits <- nrow(.queryCpos(i,x))
  hits
}
)



#' split partition into partitionCluster
#' 
#' Split a partition object into a partition Cluster if gap between strucs
#' exceeds a minimum number of tokens specified by 'gap'. Relevant to 
#' split up a plenary protocol into speeches. Note: To speed things up, the
#' returned partitions will not include frequency lists. The lists can be
#' prepared by applying \code{enrich} on the partitionCluster object that
#' is returned.
#' 
#' @param x a partition object
#' @param gap an integer specifying the minimum gap for performing the split
#' @param drop not yet implemented
#' @param ... further arguments
#' @return a partitionCluster
#' @aliases split,partition
#' @rdname split-partition-method 
#' @exportMethod split
#' @docType methods
setMethod("split", "partition", function(x, gap, drop=FALSE, ...){
  # if (length(x@metadata) == 0) warning("no metadata, method potentially fails -> please check what happens")
  cpos <- x@cpos
  if (nrow(cpos) > 1){
    distance <- cpos[,1][2:nrow(cpos)] - cpos[,2][1:(nrow(cpos)-1)]
    beginning <- c(1, ifelse(distance>gap, 1, 0))
    no <- vapply(1:length(beginning), FUN.VALUE=1, function(x) ifelse (beginning[x]==1, sum(beginning[1:x]), 0))
    for (i in (1:length(no))) no[i] <- ifelse (no[i]==0, no[i-1], no[i])
    strucsClassified <- cbind(x@strucs, no)
    strucList <- split(strucsClassified[,1], strucsClassified[,2])
    cposClassified <- cbind(cpos, no)
    cposList1 <- split(cposClassified[,1], cposClassified[,3])
    cposList2 <- split(cposClassified[,2], cposClassified[,3])
    clusterRaw <- lapply(c(1:length(strucList)), function(i) {
      p <- new("partition")
      p@strucs <- strucList[[i]]
      p@cpos <- cbind(cposList1[[i]], cposList2[[i]])
      p@corpus <- x@corpus
      p@encoding <- x@encoding
      p@sAttributes <- x@sAttributes
      p@explanation <- c("partition results from split, sAttributes do not necessarily define partition")
      p@xml <- x@xml
      p@sAttributeStrucs <- x@sAttributeStrucs
      p@label <- paste(x@label, i, collapse="_", sep="")
      if (is.null(names(x@metadata))){
        meta <- NULL
      } else {
        meta <- colnames(x@metadata$table)
      }
      p <- enrich(
        p, size=TRUE,
        tf=NULL,
        meta=meta,
        verbose=TRUE
      )
      p
    })
  } else {
    clusterRaw <- list(x)
  }
  names(clusterRaw) <- unlist(lapply(clusterRaw, function(y) y@label))
  cluster <- as.partitionCluster(clusterRaw)
  cluster
})
