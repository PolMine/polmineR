#' @include partition.R features.R context.R kwic.R S4classes.R
NULL

#' Mail result.
#' 
#' Send out a mail with the statistics of an object attached as an xlsx-file.
#' 
#' @param .Object The object to deliver.
#' @param to The recipient of the mail message.
#' @param rows The number of rows of the table (if NULL, the whole table will be sent).
#' @param filename The filename of the (temporary) xlsx-file that is generated.
#' @param ... Further parameters.
#' @aliases mail mail-method
#' @rdname mail-method
#' @exportMethod mail
setGeneric("mail", function(.Object, ...) standardGeneric("mail") )



.mail <- function(msg, to = getOption("polmineR.email")){
  if (to == "") stop("email is not set in session settings")
  sendmailR::sendmail_options(list(smtpServer = getOption("polmineR.smtp_server"), smtpPort = getOption("polmineR.smtp_port")))
  sendmailR::sendmail(
    from = to,
    to = to, subject = 'polmineR delivers', msg = msg
  )
}




#' @rdname mail-method
#' @docType methods
setMethod("mail", "textstat", function(.Object, to = getOption("polmineR.email"), rows = 1L:min(250L, nrow(.Object))){
  filename <- file.path(
    tempdir(),
    if (length(.Object@name) > 0) paste(.Object@name, "xlsx", sep = ".") else paste(is(.Object)[1], "xlsx", sep = ".")
    )
  mail(.Object@stat, to = to, filename = filename, rows = rows)
})


#' @rdname mail-method
#' @docType methods
setMethod("mail", "data.frame", function(.Object, to = getOption("polmineR.email"), filename = tempfile(fileext = ".xlsx"), rows = 1L:min(250L, nrow(.Object))){
  if (!requireNamespace("sendmailR", quietly = TRUE)) {
    stop("The 'sendmailR'-package cannnot be loaded but is necessary for this method.")
  }
  store(.Object, filename = filename, rows = rows)
  mail_msg <- "Prepared and delivered by polmineR.\n"
  msg <- list(mail_msg, sendmailR::mime_part(filename))
  retval <- .mail(msg, to)$msg
  unlink(filename)
  retval
})


#' @rdname mail-method
#' @docType methods
setMethod("mail", "kwic", function(.Object, to = getOption("polmineR.email"), rows = 1L:min(250L, nrow(.Object))){
  filename <- file.path(tempdir(), "kwic.xlsx")
  mail(.Object@table, to = to, filename = filename, rows = rows)
})
