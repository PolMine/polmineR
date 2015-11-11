# #' Size of sub-partitions
# #' @param Partition a partition object
# #' @param rows what to find in rows
# #' @param cols what to find in cols
# #' @noRd
# .crosstabulationSizes <- function(Partition, rows, cols){
#   strucSize= Partition@cpos[,2] - Partition@cpos[,1] + 1
#   sAttrRows <- paste(Partition@corpus,'.', rows, sep='')
#   sAttrCols <- paste(Partition@corpus,'.', cols, sep='')
#   tab <- data.frame(
#     strucSize,
#     rows=Partition@metadata$table[,rows],
#     cols=Partition@metadata$table[,cols]
#   )
#   ctab <- xtabs(strucSize~rows+cols, data=tab)
#   ctab <- data.frame(as.matrix(unclass(ctab)))
#   colnames(ctab)[which(colnames(ctab)=="NA.")] <- "NA"
#   rownames(ctab)[which(colnames(ctab)=="NA.")] <- "NA"
#   ctab
# }


# .map <- function(tableToMatch, tableToAdjust) {
#   colnames(tableToMatch) <- sub('X(\\d)', '\\1', colnames(tableToMatch))
#   rownames(tableToAdjust)[which(rownames(tableToAdjust)=="")] <- 'NA'
#   rownames(tableToMatch)[which(rownames(tableToMatch)=="")] <- 'NA'
#   colnames(tableToAdjust)[which(colnames(tableToAdjust)=="V1")] <- 'NA'
#   colnames(tableToMatch)[which(colnames(tableToMatch)=="V1")] <- 'NA'  
#   tableToAdjust <- merge(t(tableToMatch), t(tableToAdjust), by.x="row.names", by.y="row.names", all.x=TRUE, all.y=TRUE)
#   tableToAdjust <- tableToAdjust[,(nrow(tableToMatch)+2):ncol(tableToAdjust)]
#   tableToAdjust <- t(tableToAdjust)
#   rownames(tableToAdjust) <- sub('(.*?)\\.y', '\\1', rownames(tableToAdjust))
#   tableToAdjust <- merge(tableToMatch, tableToAdjust, by.x="row.names", by.y="row.names", all.x=TRUE, all.y=TRUE)
#   tableToAdjust <- tableToAdjust[,grep('V\\d+', colnames(tableToAdjust))]
#   dimnames(tableToAdjust) <- dimnames(tableToMatch)
#   colnames(tableToAdjust) <- gsub('^X(.*?)', '\\1', colnames(tableToAdjust))
#   tableToAdjust
# }
# 

# #' @docType methods
# #' @exportMethod trim
# #' @rdname trim-method
# setMethod("trim", "partition", function(object, pAttribute, minFrequency=0, posFilter=NULL,  tokenFilter=NULL, ...){
#   rework <- object
#   if (length(pAttribute) > 1) warning("taking only one pAttribute at a time")
#   message("Trimming partition ", rework@name)
#   if (!is.null(posFilter)) {
#     if (! pAttribute %in% names(object@pos) ){
#       message("... pos need to be added first")
#       rework <- addPos(rework, pAttribute)
#     }
#     rework@pos[[pAttribute]] <- rework@pos[[pAttribute]][rework@pos[[pAttribute]] %in% posFilter]
#     rework@tf <- rework@tf[rownames(rework@tf) %in% names(rework@pos[[pAttribute]]),]
#   }
#   if (minFrequency > 0){
#     rework@tf <- rework@tf[which(rework@tf[,"tf"] >= minFrequency),]
#     rework@pos[[pAttribute]] <- rework@pos[[pAttribute]][names(rework@pos[[pAttribute]]) %in% rownames(rework@tf)]
#   }
#   if(!is.null(tokenFilter)) {
#     tokenFilter <- .adjustEncoding(tokenFilter, rework@encoding)
#     rework@tf <- rework@tf[which(rownames(rework@tf) %in% tokenFilter),]
#   }
#   rework 
# })
# 

