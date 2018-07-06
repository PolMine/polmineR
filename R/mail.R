#' @include partition.R features.R context.R kwic.R S4classes.R
NULL

#' Mail result.
#' 
#' Mail a result (to yourself).
#' 
#' @param object object to deliver
#' @param what what to send (defaults to "html")
#' @param to the receiver of the mail message
#' @param nrow the number of rows of the table (if NULL, the whole table will be sent)
#' @param fileFormat csv or xlsx, or both
#' @param filename filename
#' @param ... further parameters
#' @aliases mail mail-method
#' @rdname mail-method
#' @exportMethod mail
setGeneric("mail", function(object, ...){standardGeneric("mail")})


.attachTables <- function(tab, nrow, msg, filename, fileFormat){
  tabTempDir <- tempdir()
  if ("csv" %in% fileFormat) {
    tabFilenameCsv <- file.path(tabTempDir, paste(filename, ".csv", sep=""))
    write.csv(tab[c(1:nrow),], file=tabFilenameCsv, fileEncoding = "latin1")
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

.mail <- function(msg, to = NULL){
  server <- getOption("polmineR.smtpServer")
  smtpPort <- getOption("polmineR.smtpPort")
  sendmailR::sendmail_options(list(smtpServer = server, smtpPort = smtpPort))
  if (is.null(to)){
    to <- getOption("polmineR.email")
    if (to == "") warning("email is not set in session settings")
  }
  sendmailR::sendmail(
    from = getOption("polmineR.email"),
    to = to, subject = 'polmineR delivers', msg = msg
  )
}

#' @rdname mail-method
#' @docType methods
setMethod("mail", "partition", function(object, to=NULL, filename="drillerExport.html", what="html"){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    msg <- list('Prepared and delivered by polmineR.\n')
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
setMethod("mail", "cooccurrences", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    if(is.null(nrow)) nrow <- nrow(object@stat)
    msg <- list('Prepared and delivered by polmineR.\n')
    msg <- .attachTables(object@stat, nrow, msg, "features", fileFormat)
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
setMethod("mail", "features", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    if(is.null(nrow)) nrow <- nrow(object@stat)
    msg <- list('Prepared and delivered by polmineR.\n')
    msg <- .attachTables(object@stat, nrow, msg, "features", fileFormat)
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
setMethod("mail", "kwic", function(object, to = NULL, nrow = NULL, fileFormat = c("csv", "xlsx")){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    msg <- list('Prepared and delivered by polmineR.\n')
    if (is.null(nrow)) nrow <- nrow(object@table)
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
setMethod("mail", "data.frame", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if (requireNamespace("sendmailR", quietly = TRUE)) {
    msg <- list('Prepared and delivered by polmineR.\n')
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

