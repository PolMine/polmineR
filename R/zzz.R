if (Sys.getenv("CORPUS_REGISTRY") == "") Sys.setenv("CORPUS_REGISTRY" = "/")

default_templates <- list(
  
  plain = list(
    document = list(
      sAttribute = "text",
      format = c("### ", "")
    )
  ),
  
  article = list(
    document = list(
      sAttribute = "text",
      format = c("### ", "")
    ),
    paragraphs = list(
      sAttribute = "p_type",
      format = list(
        meta = c("### ", ""),
        title = c("## ", ""),
        teaser = c("_", "_\n"),
        body = c("", "\n"),
        highlight = c("_", "_\n"),
        headline = c("# ", "")
      )
    )
  ),
  
  plpr = list(
    metadata = c("text_speaker", "text_date", "text_party"),
    document = list(
      sAttribute = "text",
      format = c("\n### ", "\n")
    ),
    speech = list(
      sAttribute = "text_type",
      format = list(
        speech = c("", ""),
        interjection = c("\n> ", "\n")
      )
    )
  )
  
)



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
    "polmineR.smtpServer" = "",
    "polmineR.smtpPort" = "",
    "polmineR.email" = "",
    "polmineR.partitionDir" = "",
    "polmineR.browse" = FALSE,
    "polmineR.backend" = "doSNOW",
    "polmineR.specialChars" = "^[a-zA-Z\u00e9\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc-\u00df|-]+$",
    "polmineR.interface" = "rcqp",
    "polmineR.template" = "default",
    "polmineR.templates" = default_templates,
    "polmineR.cutoff" = 5000,
    "polmineR.Rcpp" = TRUE,
    "polmineR.cwb-s-decode" = FALSE,
    "polmineR.cwb-encode" = FALSE,
    "polmineR.cwb-lexdecode" = FALSE
  )
  
  
  # Some operations run faster when using the utils of the CWB (cwb-s-decode etc.)
  # These may only be available on *nix systems, so this is checked first
  if (.Platform$OS.type == "unix"){
    if (system("cwb-s-decode -h", intern = FALSE, ignore.stderr =  TRUE) == 1){
      options("polmineR.cwb-s-decode" = TRUE)
    }
  }
  
  if (.Platform$OS.type == "unix"){
    if (system("cwb-encode -h", intern = FALSE, ignore.stderr =  TRUE) == 2){
      options("polmineR.cwb-encode" = TRUE)
    }
  }

  if (.Platform$OS.type == "unix"){
    if (system("cwb-lexdecode -h", intern = FALSE, ignore.stderr =  TRUE) == 2){
      options("polmineR.cwb-lexdecode" = TRUE)
    }
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
  if (require("rcqp")){
    packageStartupMessage("Using the rcqp package as interface to access CWB corpora")
    CQI <- CQI.rcqp$new()
  } else {
    packageStartupMessage("Using perl scripts as interface to access CWB corpora")
    CQI <- CQI.perl$new()
  }
}
