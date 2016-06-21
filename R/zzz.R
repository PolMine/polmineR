.onLoad <- function (lib, pkg) {
  
  options(
    "polmineR.project" = c(""),
    "polmineR.projectDir" = c(""),
    "polmineR.corpus" = "PLPRBTTXT",
    "polmineR.pAttribute" = "word",
    "polmineR.left" = 5,
    "polmineR.right" = 5,
    "polmineR.minSignificance" = 3.84,
    "polmineR.minFrequency" = 5,
    "polmineR.filterType" = "include",
    "polmineR.lineview" = FALSE,
    "polmineR.meta " =  as.character(c()),
    "polmineR.mc" = FALSE,
    "polmineR.cores" = 2,
    "polmineR.consoleEncoding" = "UTF-8",
    "polmineR.smtpServer" = "",
    "polmineR.smtpPort" = "",
    "polmineR.email" = "",
    "polmineR.partitionDir" = "",
    "polmineR.browse" = FALSE,
    "polmineR.backend" = "doSNOW",
    "polmineR.specialChars" = "^[a-zA-Z\u00e9\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc-\u00df|-]+$",
    "polmineR.interface" = "rcqp"
  )
  
}

# CQI <- CQI_rcqp$new()
# CQI <- cqi:::CQI$new()
# CQI$authenticate()

