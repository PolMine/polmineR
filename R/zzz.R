if (Sys.getenv("CORPUS_REGISTRY") == "") Sys.setenv("CORPUS_REGISTRY" = "/")


.onLoad <- function (lib, pkg) {
  
  if (Sys.getenv("CORPUS_REGISTRY") == "") Sys.setenv("CORPUS_REGISTRY" = "/")
  
  options(
    "polmineR.project" = "",
    "polmineR.projectDir" = "",
    "polmineR.corpus" = "PLPRBTTXT",
    "polmineR.pAttribute" = "word",
    "polmineR.left" = 5,
    "polmineR.right" = 5,
    "polmineR.minSignificance" = 3.84,
    "polmineR.minFrequency" = 5,
    "polmineR.filterType" = "include",
    "polmineR.lineview" = FALSE,
    "polmineR.meta " =  character(),
    "polmineR.mc" = FALSE,
    "polmineR.cores" = 2,
    "polmineR.smtpServer" = "",
    "polmineR.smtpPort" = "",
    "polmineR.email" = "",
    "polmineR.partitionDir" = "",
    "polmineR.browse" = FALSE,
    "polmineR.backend" = "doSNOW",
    "polmineR.specialChars" = "^[a-zA-Z\u00e9\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc-\u00df|-]+$",
    "polmineR.interface" = "rcqp",
    "polmineR.template" = "default",
    "polmineR.templates" = list(),
    "polmineR.cutoff" = 5000,
    "polmineR.Rcpp" = TRUE,
    "polmineR.cwb-s-decode" = FALSE,
    "polmineR.cwb-encode" = FALSE,
    "polmineR.cwb-lexdecode" = FALSE,
    "polmineR.cwb-regedit" = FALSE,
    "polmineR.defaultRegistry" = Sys.getenv("CORPUS_REGISTRY")
  )
  
  
  # Some operations run faster when using the utils of the CWB (cwb-s-decode etc.)
  # These may only be available on *nix systems, so this is checked first
  if (.Platform$OS.type == "unix"){
    if (system("cwb-s-decode -h", intern = FALSE, ignore.stderr =  TRUE) == 1)
      options("polmineR.cwb-s-decode" = TRUE)
    if (system("cwb-encode -h", intern = FALSE, ignore.stderr =  TRUE) == 2)
      options("polmineR.cwb-encode" = TRUE)
    if (system("cwb-lexdecode -h", intern = FALSE, ignore.stderr =  TRUE) == 2)
      options("polmineR.cwb-lexdecode" = TRUE)
    if (system("cwb-regedit -h", intern = FALSE, ignore.stderr = TRUE) == 255)
      options("polmineR.cwb-regedit" = TRUE)
    
  }
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

#' @importFrom utils packageVersion
.onAttach <- function(lib, pkg){
  if (Sys.getenv("CORPUS_REGISTRY") == "") Sys.setenv("CORPUS_REGISTRY" = "/")
  setTemplate()
  getSettings()
  packageStartupMessage(sprintf("polmineR %s", packageVersion("polmineR")))
  
  if (Sys.getenv("CORPUS_REGISTRY") %in% c("", "/")){
    packageStartupMessage(
      "The CORPUS_REGISTRY environment variable is not defined. ",
      "See the package vignette to learn how to set it!"
    )
  } else {
    packageStartupMessage("registry:  ", getOption("polmineR.defaultRegistry"))
  }
  
  packageStartupMessage("interface: ", class(CQI)[1])
  
}
