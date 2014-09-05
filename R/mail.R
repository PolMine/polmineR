#' @include methods.R keyness.R
NULL

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

