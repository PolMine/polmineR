#' @include partition.R features.R context.R kwic.R S4classes.R
NULL

#' Send the result of an analysis by Email.
#' 
#' Send out a mail with the statistical analysis included in an object attached
#' as an xlsx-file.
#' 
#' The method translates the result table in the object provided into an Excel sheet
#' and attaches the sheet to an Email which will be sent to the Email-address provided
#' by the argument \code{to}. A pre-requirement is that the global options \code{polmineR.smtp_port} and
#' \code{polmineR.smtp_server} are validly defined. See examples.
#' 
#' Please note: At this stage, authentication is not yet supported.
#' 
#' @param .Object The object to deliver.
#' @param to An email-address, the recipient of the mail message.
#' @param rows The number of rows of the table included in the Excel file to be
#'   sent (if \code{NULL}, the whole table will be sent).
#' @param filename The filename of the (temporary) xlsx-file that is generated.
#' @param ... Further parameters.
#' @aliases mail mail-method
#' @rdname mail-method
#' @exportMethod mail
#' @examples
#' # Get all (global) options for the polmineR package
#' grep("polmineR", names(options()), value = TRUE)
#' 
#' # Get options that need to be set
#' getOption("polmineR.email")
#' getOption("polmineR.smtp_server")
#' getOption("polmineR.smtp_port")
#' 
#' # Sample options (let us imagine Donald Duck had a mail-account)
#' options("polmineR.email" = "donald.duck@@duckmail.org")
#' options("polmineR.smtp_port" = "587")
#' options("polmineR.smtp_server" = "smtp.duckmail.org")
#' 
#' # This is how you send out results when options are set
#' # (Note: Mail servers that require authentication are not yet supported.)
#' \dontrun{
#' y <- cooccurrences("REUTERS", query = "oil")
#' mail(y)
#' 
#' k <- kwic("REUTERS", query = "oil")
#' mail(k)
#' }
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
    normalizePath(tempdir(), winslash = "/"),
    if (length(.Object@name) > 0) paste(.Object@name, "xlsx", sep = ".") else paste(is(.Object)[1], "xlsx", sep = "."),
    fsep = "/"
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
  filename <- file.path(normalizePath(tempdir(), winslash = "/"), "kwic.xlsx", fsep = "/")
  mail(.Object@table, to = to, filename = filename, rows = rows)
})
