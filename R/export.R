#' @include generics.R keyness.R
NULL

.mail <- function(msg, to){
  server <- get("drillingControls", '.GlobalEnv')[['smtpServer']]
  smtpPort <- get("drillingControls", '.GlobalEnv')[['smtpPort']]
  sendmail_options(list(smtpServer=server, smtpPort=smtpPort))
  if (is.null(to)){
    to <- get("drillingControls", '.GlobalEnv')[['email']]
    if (to == "") warning("email is not set in drillingControls")
  }
  sendmail(from=to,
           to=to,
           subject='driller message',
           msg=msg
  )
}

#' mail result of keyness analysis
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
#' @name mail-keyness-method
#' @rdname keyness-mail-method
#' @aliases mail,keyness-method
#' @docType methods
setMethod("mail", "keyness", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if(is.null(nrow)) nrow <- nrow(object@stat)
  msg <- list('Delivering Tables.\nSincerely yours\nThe driller\n')
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
  .mail(msg, to)
})

#' mail a result
#' 
#' still experimental
#' 
#' @param object an object with some statistics
#' @param to the receiver of the mail message
#' @param filename name of the file to be sent out
#' @param what what to send (defaults to "html")
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @include statistics.R
#' @name mail,partition-method
#' @rdname mail-partition-method
#' @aliases mail,partition-method
#' @docType methods
setMethod("mail", "partition", function(object, to=NULL, filename="drillerExport.html", what="html"){
  msg <- list('Delivering something to read.\nSincerely yours\nThe driller\n')
  filename <- html(
    object, meta=NULL, browser=FALSE,
    filename=file.path(tempdir(), filename)
    )
  msg[[length(msg)+1]] <- mime_part(filename)
  .mail(msg, to)
})

#' mail concordances
#' 
#' still experimental
#' 
#' @param object the concordance object
#' @param to the receiver of the mail message
#' @param nrow the number of rows
#' @param fileFormat csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @include kwic.R
#' @name mail-concordances-method
#' @rdname keyness-concordances-method
#' @aliases mail,concordances-method
#' @docType methods
setMethod("mail", "concordances", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  msg <- list('Delivering concordances.\nSincerely yours\nThe driller\n')
  tabTempDir <- tempdir()
  if ("csv" %in% fileFormat) {
    tabFilenameCsv <- file.path(tabTempDir, "concordances.csv")
    write.csv(object@table, file=tabFilenameCsv, fileEncoding="latin1")
    msg[[length(msg)+1]] <- mime_part(tabFilenameCsv)
  }
  if ("xlsx" %in% fileFormat) {
    tabFilenameXlsx <- file.path(tabTempDir, "concordances.xlsx")
    write.xlsx(object@table, file=tabFilenameXlsx)
    msg[[length(msg)+1]] <- mime_part(tabFilenameXlsx)
  }
  .mail(msg, to)
})
