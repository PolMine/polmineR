# setpaths.R - last modification: 2018-05-09
# author: Andreas Blaette (andreas.blaette@uni-due.de)
# 
#
# This is a standardized R script to perform the necessary configuration the
# path to the data directory and the info file in the registry files of CWB
# indexed corpora that are included in a R data package upon installing the
# package.
# 
# The registry files are assumed to be in the subdirectory 
# inst/extdata/cwb/registry of the data package. The indexed corpora and the 
# info file are assumed to be in the directory inst/extdata/cwb/indexed_corpora.
# 
# The script should be put in package subdirectory 'tools' of the data package.
# It is assumed to be called from the configure scripts of a R package
# (configure/configure.win).
#
# On Linux/macOS, include a shell script configure with this line:
# ${R_HOME}/bin/Rscript ./tools/setpaths.R --args "$R_PACKAGE_DIR" 
#
# On Windows, include a shell script configure.win with this line:
# ${R_HOME}/bin${R_ARCH_BIN}/Rscript.exe ./tools/setpaths.R --args "$R_PACKAGE_DIR"
# 
# The latest version of this file is part of the ctk package, available
# at www.github.com/PolMine/ctk, in the directory inst/R.



# the directory where the package will be installed is passed into the
# R script as a command line argument
args <- commandArgs(trailingOnly = TRUE)
packageDir <- args[2] 

# this is where the registry directory is before copying everything to the
# final location
registryDir <- file.path(getwd(), "inst", "extdata", "cwb", "registry")

for (corpus in list.files(registryDir)){
  registryFile <- file.path(registryDir, corpus)
  registry <- readLines(registryFile)
  
  homeDir <- file.path(packageDir, "extdata", "cwb", "indexed_corpora", corpus)
  infoFileLine <- grep("^INFO", registry)
  infoFileBasename <- basename(gsub('^INFO\\s+(.*?)"*\\s*$', "\\1", registry[infoFileLine]))
  infoFileNew <- file.path(homeDir, infoFileBasename)
  
  # On Windows, the CWB tools will digest the path including the volume
  # declaration only if the path declaration is quoted; on macOS, wrapping the
  # path is only necessary, if there is a whitespace in the path
  if (.Platform$OS.type == "windows"){
    registry[grep("^HOME", registry)] <- sprintf('HOME "%s"', homeDir)
    registry[infoFileLine] <- sprintf('INFO "%s"', infoFileNew)
  } else {
    if (grepl(" ", homeDir)){
      registry[grep("^HOME", registry)] <- sprintf('HOME "%s"', homeDir)
      registry[infoFileLine] <- sprintf('INFO "%s"', infoFileNew)
    } else {
      registry[grep("^HOME", registry)] <- sprintf("HOME %s", homeDir)
      registry[infoFileLine] <- sprintf("INFO %s", infoFileNew)
    }
  }
  
  writeLines(text = registry, con = registryFile, sep = "\n")
}
