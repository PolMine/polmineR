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


if (Sys.getenv("POLMINER_INTERFACE") == "rcqp"){
  packageStartupMessage("Using the rcqp package as interface to access CWB corpora")
  CQI <- CQI.cqpserver$new()
} else if (Sys.getenv("POLMINER_INTERFACE") == "perl"){
  packageStartupMessage("Using perl scripts as interface to access CWB corpora")
  CQI <- CQI.cqpserver$new()
} else if (Sys.getenv("POLMINER_INTERFACE") == "cqpserver"){
  packageStartupMessage("Using cqpserver as interface to access CWB corpora")
  CQI <- CQI.cqpserver$new()
} else if (Sys.getenv("POLMINER_INTERFACE") == ""){
  if (require("rcqp", quietly = TRUE)){
    packageStartupMessage("Using the rcqp package as interface to access CWB corpora")
    CQI <- CQI.rcqp$new()
  } else {
    packageStartupMessage("Using perl scripts as interface to access CWB corpora")
    CQI <- CQI.perl$new()
  }
}
