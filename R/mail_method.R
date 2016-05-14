#' @include partition_class.R comp_class.R context_class.R kwic_class.R
NULL

#' mail result
#' 
#' Mail a result (to yourself).
#' 
#' @param object a driller object
#' @param what what to send (defaults to "html")
#' @param to the receiver of the mail message
#' @param nrow the number of rows of the table (if NULL, the whole table will be sent)
#' @param fileFormat csv or xlsx, or both
#' @param filename filename
#' @param ... further parameters
#' @aliases mail mail-method
#' @rdname mail-method
setGeneric("mail", function(object, ...){standardGeneric("mail")})


.attachTables <- function(tab, nrow, msg, filename, fileFormat){
  tabTempDir <- tempdir()
  if ("csv" %in% fileFormat) {
    tabFilenameCsv <- file.path(tabTempDir, paste(filename, ".csv", sep=""))
    write.csv(tab[c(1:nrow),], file=tabFilenameCsv, fileEncoding="latin1")
    msg[[length(msg)+1]] <- sendmailR::mime_part(tabFilenameCsv)
  }
  if ("xlsx" %in% fileFormat) {
    if (requireNamespace("xlsx", quietly=T)){
      tabFilenameXlsx <- file.path(tabTempDir, paste(filename, ".xlsx", sep=""))
      xlsx::write.xlsx(tab[c(1:nrow),], file=tabFilenameXlsx)
      msg[[length(msg)+1]] <- sendmailR::mime_part(tabFilenameXlsx)      
    } else {
      warning("the 'xlsx'-packge needs to be installed to mail xlsx-sheets")
      stop()
    }
  }
  msg
}

.mail <- function(msg, to){
  server <- getOption("polmineR.smtpServer")
  smtpPort <- getOption("polmineR.smtpPort")
  sendmailR::sendmail_options(list(smtpServer=server, smtpPort=smtpPort))
  if (is.null(to)){
    to <- getOption("polmineR.email")
    if (to == "") warning("email is not set in session settings")
  }
  sendmailR::sendmail(
    from=getOption("polmineR.email"),
    to=to, subject='driller message', msg=msg
  )
}

#' @rdname mail-method
#' @docType methods
setMethod("mail", "partition", function(object, to=NULL, filename="drillerExport.html", what="html"){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    msg <- list('Delivering something to read.\nSincerely yours\nThe driller\n')
    filename <- html(
      object, meta=NULL, browser=FALSE,
      filename=file.path(tempdir(), filename)
    )
    msg[[length(msg)+1]] <- sendmailR::mime_part(filename)
    status <- .mail(msg, to)
    retval <- status$msg
  } else {
    warning("the sendmailR package cannnot be loaded but is necessary for this method")
    stop()
  }
  retval
})




#' @rdname mail-method
#' @docType methods
setMethod("mail", "context", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    if(is.null(nrow)) nrow <- nrow(object@stat)
    msg <- list('Delivering Tables.\nSincerely yours\nThe driller\n')
    msg <- .attachTables(object@stat, nrow, msg, "comp", fileFormat)
    status <- .mail(msg, to)
    retval <- status$msg
  } else {
    warning("the sendmailR package cannnot be loaded but is necessary for this method")
    stop()
  }
  retval
})


#' @rdname mail-method
#' @docType methods
setMethod("mail", "comp", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    if(is.null(nrow)) nrow <- nrow(object@stat)
    msg <- list('Delivering Tables.\nSincerely yours\nThe driller\n')
    msg <- .attachTables(object@stat, nrow, msg, "comp", fileFormat)
    status <- .mail(msg, to)
    retval <- status$msg  
  } else {
    warning("the sendmailR package cannnot be loaded but is necessary for this method")
    stop()
  }
  retval
})


#' @rdname mail-method
#' @docType methods
setMethod("mail", "kwic", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    msg <- list('Delivering kwic.\nSincerely yours\nThe driller\n')
    if(is.null(nrow)) nrow <- nrow(object@stat)
    msg <- .attachTables(object@table, nrow, msg, "kwic", fileFormat) 
    status <- .mail(msg, to)
    retval <- status$msg
  } else {
    warning("the sendmailR package cannnot be loaded but is necessary for this method")
    stop()
  }
  retval
})

#' @rdname mail-method
#' @docType methods
setMethod("mail", "dispersion", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    msg <- list('Delivering a crosstabulation.\nSincerely yours\nThe driller\n')
    if(is.null(nrow)) nrow <- nrow(object@abs)
    msg <- .attachTables(object@abs, nrow, msg, "crosstabAbs", fileFormat) 
    msg <- .attachTables(object@rel, nrow, msg, "crosstabRel", fileFormat) 
    status <- .mail(msg, to)
    retval <- status$msg  
  } else {
    warning("the sendmailR package cannnot be loaded but is necessary for this method")
    stop()
  }
  retval
})

#' @rdname mail-method
#' @docType methods
setMethod("mail", "data.frame", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    msg <- list('Delivering a data frame.\nSincerely yours\nThe driller\n')
    if(is.null(nrow)) nrow <- nrow(object)
    msg <- .attachTables(object, nrow, msg, "dataFrame", fileFormat) 
    status <- .mail(msg, to)
    retval <- status$msg
  } else {
    warning("the sendmailR package cannnot be loaded but is necessary for this method")
    stop()
  }
  retval  
})

