if (Sys.getenv("CORPUS_REGISTRY") == "") Sys.setenv("CORPUS_REGISTRY" = "/")

.onLoad <- function (lib, pkg) {
  
  if (Sys.getenv("CORPUS_REGISTRY") == "") Sys.setenv("CORPUS_REGISTRY" = "/")
  
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
    "polmineR.interface" = "rcqp",
    "polmineR.template" = "default",
    "polmineR.cutoff" = 5000
  )
}

getSettings <- function(){
  if (length(Sys.getenv("POLMINER_DIR"))){
    configFilename <- file.path(Sys.getenv("POLMINER_DIR"), "polmineR.conf")
    if (file.exists(configFilename)){
      configDataRaw <- scan(configFilename, what = character(), sep = "\n", quiet = TRUE)
      # remove lines with comments
      commentLines <- grep("^\\s*#", configDataRaw)
      if (length(commentLines) > 0) configDataRaw <- configDataRaw[-commentLines]
      # remove comments
      configDataRaw <- gsub("\\s*#.*?$", "", configDataRaw)
      
      # remove whitespace
      settingsList <- lapply(
        strsplit(configDataRaw, "[=|:)]"),
        function(x) unname(sapply(x, function(y) gsub("\\s", "", y)))
      )
      # drop value pairs with length <> 2
      settingsList <- lapply(which(sapply(settingsList, length) == 2), function(x) settingsList[[x]])
      # turn into named list
      newSettings <- as.list(sapply(settingsList, function(x) x[2]))
      newSettings <- lapply(
        newSettings,
        function(x){
          if (grepl("^\\d+$", x)){
            return(as.numeric(x))
          } else {
            return(x)
          }
        }
      )
      names(newSettings) <- paste("polmineR", sapply(settingsList, function(x) x[1]), sep = ".")
      options(newSettings)
    }
  }
}

.onAttach <- function(lib, pkg){
  if (Sys.getenv("CORPUS_REGISTRY") == "") Sys.setenv("CORPUS_REGISTRY" = "/")
  setTemplate()
  getSettings()
}


if (Sys.getenv("POLMINER_INTERFACE") == "rcqp"){
  packageStartupMessage("Using the rcqp package as interface to access CWB corpora")
  CQI <- CQI.rcqp$new()
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
