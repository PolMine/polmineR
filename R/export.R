#' @include generics.R keyness.R
NULL

#' mail a result
#' 
#' still experimental
#' 
#' @param object an object with some statistics
#' @param to the receiver of the mail message
#' @param nrow the number of rows
#' @param fileFormat either csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @include statistics.R
#' @name mail
#' @rdname mail-method
#' @aliases mail mail,keyness-method
#' @docType methods
setMethod("mail", "keyness", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if(is.null(nrow)) nrow <- nrow(object@stat)
  server <- get("drillingControls", '.GlobalEnv')[['smtpServer']]
  smtpPort <- get("drillingControls", '.GlobalEnv')[['smtpPort']]
  sendmail_options(list(smtpServer="mailout.uni-due.de", smtpPort="587"))
  if (is.null(to)){
    to <- get("drillingControls", '.GlobalEnv')[['email']]
  }
  msg <- list(
    'Delivering Tables.\nSincerely yours\nThe driller\n'
  )
  tabTempDir <- tempdir()
  if ("csv" %in% fileFormat) {
    tabFilenameCsv <- file.path(tabTempDir, "drillerExport.csv")
    write.csv(object@stat[c(1:nrow),], file=tabFilenameCsv, fileEncoding="latin1")
    msg[[length(msg)+1]] <- mime_part(tabFilenameCsv)
  }
  if ("xlsx" %in% fileFormat) {
    tabFilenameXlsx <- file.path(tabTempDir, "drillerExport.xlsx")
    write.xlsx(object@stat[c(1:nrow),], file=tabFilenameXlsx)
    msg[[length(msg)+1]] <- mime_part(tabFilenameXlsx)
  }
  sendmail(from=to,
           to=to,
           subject='driller message',
           msg=msg
  )
})

