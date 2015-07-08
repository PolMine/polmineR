# 
# #' @examples 
# #' \dontrun{
# #' sz2010 <- partition("ARENEN", list(text_date=".*2010"), method="grep", tf=NULL)
# #' dates <- sAttributes(sz2010, "text_date")
# #' sz2010 <- partitionBundle("ARENEN", def=list(text_date=".*2010"), var=list(text_date=dates), tf="word")
# #' sz2010tdm <- as.TermDocumentMatrix(sz2010)
# #' }
# setMethod("ctm", "TermDocumentMatrix", function(.Object){
#   docLengths <- tapply(.Object$v, .Object$j, sum)
#   splittedByDoc <- split(
#     x=data.frame(vocab=.Object$i, tf=.Object$v),
#     f=.Object$j
#   )
#   idAndTf <- lapply(splitted, function(docTf){
#     paste(apply(as.matrix(docTf), 1, function(row) paste(row, collapse=":", sep="")), collapse=" ")
#   })
#   ctmData <- paste(
#     mapply(function(x,y) paste(x, y, sep=" "), docLengths, idAndTf),
#     collapse="\n"
#   )
#   ctmTmpDir <- tempdir()
#   # dir.create(file.path(ctmTmpDir), "ctmData")
#   cat(ctmData, file=file.path(ctmTmpDir, "ctmData.txt"))
# })
# 

