#' @include partition-class.R keyness-class.R context-class.R kwic-class.R crosstab-class.R
NULL

#' mail result
#' 
#' Mail a result (to yourself).
#' Please see the respective documentation for details 
#' (\code{method?mail("keyness")}, \code{method?mail("partition")}.
#' 
#' @param object a driller object
#' @param ... further parameters
#' @aliases mail mail-method
#' @rdname mail
setGeneric("mail", function(object, ...){standardGeneric("mail")})


#' @importFrom sendmailR sendmail sendmail_options mime_part
#' @importFrom xlsx write.xlsx
.attachTables <- function(tab, nrow, msg, filename, fileFormat){
  tabTempDir <- tempdir()
  if ("csv" %in% fileFormat) {
    tabFilenameCsv <- file.path(tabTempDir, paste(filename, ".csv", sep=""))
    write.csv(tab[c(1:nrow),], file=tabFilenameCsv, fileEncoding="latin1")
    msg[[length(msg)+1]] <- mime_part(tabFilenameCsv)
  }
  if ("xlsx" %in% fileFormat) {
    tabFilenameXlsx <- file.path(tabTempDir, paste(filename, ".xlsx", sep=""))
    write.xlsx(tab[c(1:nrow),], file=tabFilenameXlsx)
    msg[[length(msg)+1]] <- mime_part(tabFilenameXlsx)
  }
  msg
}

#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
.mail <- function(msg, to){
  server <- get("drillingControls", '.GlobalEnv')[['smtpServer']]
  smtpPort <- get("drillingControls", '.GlobalEnv')[['smtpPort']]
  sendmail_options(list(smtpServer=server, smtpPort=smtpPort))
  if (is.null(to)){
    to <- get("drillingControls", '.GlobalEnv')[['email']]
    if (to == "") warning("email is not set in drillingControls")
  }
  sendmail(from=get("drillingControls", '.GlobalEnv')[['email']],
           to=to,
           subject='driller message',
           msg=msg
  )
}

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
  status <- .mail(msg, to)
  status$msg
})




#' mail result of context analysis
#' 
#' still experimental
#' 
#' @param object a context object
#' @param to the receiver of the mail message
#' @param nrow the number of rows
#' @param fileFormat either csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @name context-keyness-method
#' @rdname context-mail-method
#' @aliases mail,context-method
#' @docType methods
setMethod("mail", "context", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if(is.null(nrow)) nrow <- nrow(object@stat)
  msg <- list('Delivering Tables.\nSincerely yours\nThe driller\n')
  msg <- .attachTables(object@stat, nrow, msg, "keyness", fileFormat)
  status <- .mail(msg, to)
  status$msg
})


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
#' @name mail-keyness-method
#' @rdname keyness-mail-method
#' @aliases mail,keyness-method
#' @docType methods
setMethod("mail", "keyness", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if(is.null(nrow)) nrow <- nrow(object@stat)
  msg <- list('Delivering Tables.\nSincerely yours\nThe driller\n')
  msg <- .attachTables(object@stat, nrow, msg, "keyness", fileFormat)
  status <- .mail(msg, to)
  status$msg
})


#' mail kwic/concordances
#' 
#' still experimental
#' 
#' @param object the concordance object
#' @param to the receiver of the mail message
#' @param nrow the number of rows of the table (if NULL, the whole table will be sent)
#' @param fileFormat csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @name mail-kwic-method
#' @rdname mail-kwic-method
#' @aliases mail,kwic-method
#' @docType methods
setMethod("mail", "kwic", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  msg <- list('Delivering kwic.\nSincerely yours\nThe driller\n')
  if(is.null(nrow)) nrow <- nrow(object@stat)
  msg <- .attachTables(object@table, nrow, msg, "kwic", fileFormat) 
  status <- .mail(msg, to)
  status$msg
})

#' mail crosstab
#' 
#' For exporting.
#' 
#' @param object the crosstab object
#' @param to the receiver of the mail message
#' @param nrow the number of rows of the table (if NULL, the whole table will be sent)
#' @param fileFormat csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @name mail-crosstab-method
#' @rdname mail-crosstab-method
#' @aliases mail,crosstab-method
#' @docType methods
setMethod("mail", "crosstab", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  msg <- list('Delivering a crosstabulation.\nSincerely yours\nThe driller\n')
  if(is.null(nrow)) nrow <- nrow(object@abs)
  msg <- .attachTables(foo@abs, nrow, msg, "crosstabAbs", fileFormat) 
  msg <- .attachTables(foo@rel, nrow, msg, "crosstabRel", fileFormat) 
  status <- .mail(msg, to)
  status$msg
})

#' mail a data frame
#' 
#' For exporting.
#' 
#' @param object the data frame
#' @param to the receiver of the mail message
#' @param nrow the number of rows of the table (if NULL, the whole table will be sent)
#' @param fileFormat csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @name mail-data.frame-method
#' @rdname mail-data.frame-method
#' @aliases mail,data.frame-method
#' @docType methods
setMethod("mail", "data.frame", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  msg <- list('Delivering a data frame.\nSincerely yours\nThe driller\n')
  if(is.null(nrow)) nrow <- nrow(object)
  msg <- .attachTables(object, nrow, msg, "dataFrame", fileFormat) 
  status <- .mail(msg, to)
  status$msg
})

