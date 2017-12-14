#' Install Corpus Workbench
#' 
#' Install precompiled version of the Corpus Workbench in the directory
#' extdata/cwb/CWB. This is a precondition to offer full CQP functionality
#' on Windows systems.
#' @export install.cwb
#' @importFrom utils unzip untar
install.cwb <- function(){
  if (.Platform$OS.type == "unix"){
    if (Sys.info()["sysname"] == "Darwin"){
      urlToGet <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-osx-10.5-universal.tar.gz"
    } else if (Sys.info()["sysname"] == "Linux"){
      urlToGet <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-linux-x86_64.tar.gz"
    } else {
      stop("Platform is 'unix', but Sys.info()['sysname'] is neither 'Darwin' (i.e. MacOS) nor 'Linux'")
    }
    subdir <- gsub("^(.*?)\\.tar\\.gz$", "\\1", basename(urlToGet))
  } else if (.Platform$OS.type == "windows"){
    urlToGet <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.4-beta/cwb-3.4.10-windows-i586-UPDATED.zip"
    subdir <- "cwb-3.4.10-windows-i586"
  }
  
  baseDir <- system.file(package = "polmineR", "extdata", "cwb")
  cwbDir <- file.path(baseDir, "CWB")
  dir.create(cwbDir)
  
  tmpDir <- tempdir()
  oldWorkingDir <- setwd(tmpDir)
  download.file(urlToGet, destfile = basename(urlToGet))
  if (.Platform$OS.type == "windows"){
    unzip(basename(urlToGet))
  } else {
    untar(basename(urlToGet))
  }
  setwd(subdir)
  installScriptFile <- if (.Platform$OS.type == "windows") "install-cwb-win.bat" else "./install-cwb.sh"
  installScript <- readLines(installScriptFile)
  if (.Platform$OS.type == "windows"){
    cwbDir <- gsub("/", "\\\\", cwbDir)
    installScript[grep("^set PREFIX=", installScript)] <- sprintf("set PREFIX=%s", cwbDir)
  } else {
    installScript[grep("^PREFIX=", installScript)] <- sprintf("PREFIX='%s'", cwbDir)
  }
  
  cat(installScript, file = installScriptFile, sep = "\n")
  if (.Platform$OS.type == "windows") shell(installScriptFile) else system(installScriptFile)
  unlink(tmpDir)
  setwd(oldWorkingDir)
}