#' @include partition.R features.R context.R kwic.R S4classes.R
NULL

#' Send the result of an analysis by Email.
#' 
#' Send out a mail with the statistical analysis included in an object attached
#' as an xlsx-file.
#' 
#' The method translates the result table in the object provided into an Excel
#' sheet (xlsx-file) and attaches the sheet to an Email which will be sent to the
#' Email-address provided by the argument \code{to}. A pre-requirement is that
#' the global options \code{polmineR.smtp_port} and \code{polmineR.smtp_server}
#' are defined. See examples.
#' 
#' Please note: At this stage, explicit authentication is not yet supported, and
#' mail delivery will fail if authentication fails. Depending on the
#' authentication requirements of your mail server, explicit authentication is
#' not necessary if your logged in via VPN to your organisation.
#' 
#' @param .Object The object to deliver.
#' @param to An email-address, the recipient of the mail message.
#' @param rows The number of rows of the table included in the Excel file to be
#'   sent (if \code{NULL}, the whole table will be sent).
#' @param filename The filename of the (temporary) xlsx-file that is generated.
#' @param ... Further parameters.
#' @return The method returns \code{TRUE} if the message has been sent out
#'   successfully, and \code{FALSE}, if not.
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
  
  no_email <- "Mailing KWIC table not possible: No email address is set!"
  no_server <- "Mailing KWIC table not possible: smtp_server is no set!"
  no_port <- "Mailing KWIC table not possible: smtp_port is no set!"
  
  go <- TRUE
  if (nchar(to) == 0L){.message(no_email , type = "error"); go <- FALSE}
  if (nchar(options("polmineR.smtp_server")) == 0L){.message(no_server, type = "error"); go <- FALSE}
  if (nchar(options("polmineR.smtp_port")) == 0L){.message(no_port, type = "error"); go <- FALSE}
  if (!go) return(FALSE)
  
  sendmailR::sendmail_options(
    list(
      smtpServer = getOption("polmineR.smtp_server"),
      smtpPort = getOption("polmineR.smtp_port")
    )
  )
  subj <- 'polmineR delivers'
  
  success <- try(sendmailR::sendmail(from = to, to = to, subject = subj, msg = msg))
  
  if (is(success)[1] == "try-error"){
    .message(success[1], type = "error")
    retval <- FALSE
  } else {
    retval <- TRUE
  }
  retval
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
  retval <- .mail(msg, to)
  unlink(filename)
  retval
})


#' @rdname mail-method
#' @docType methods
setMethod("mail", "kwic", function(.Object, to = getOption("polmineR.email"), rows = 1L:min(250L, nrow(.Object))){
  filename <- file.path(normalizePath(tempdir(), winslash = "/"), "kwic.xlsx", fsep = "/")
  mail(as.data.frame(.Object@stat), to = to, filename = filename, rows = rows)
})
