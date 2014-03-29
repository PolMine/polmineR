.importPolMineCorpus <- function(corpus, user, pw, binaryDir=NULL){
  urlRegistry <- paste("http://polmine.sowi.uni-due.de/cwb/", tolower(corpus), "/", tolower(corpus), sep="")
  urlBinaries <- paste("http://polmine.sowi.uni-due.de/cwb/", tolower(corpus), "/", tolower(corpus), ".tar.gz", sep="")
  registryFilename <- file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(corpus))
  pathElements <- unlist(strsplit(Sys.getenv("CORPUS_REGISTRY"), split="/"))
  cwbDir <- paste(pathElements[2:(length(pathElements)-1)], collapse="/")
  cwbDir <- paste('/', cwbDir, sep='')
  dirs <- list.dirs(cwbDir, recursive=FALSE)
  binaryDir <- dirs[grep("indexed", dirs)]
  cat('... destination for corpus binary files: ', binaryDir, '\n')
  tarFilename <- file.path(binaryDir, paste(tolower(corpus), ".tar.gz", sep=""))
  download.file(
    url=urlRegistry,
    destfile=file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(corpus)),
    method="wget",
    extra=paste("--http-user=", user, " --http-passwd=", pw, sep="")
    )
  
  download.file(
     url=urlBinaries,
     destfile=tarFilename,
     method="wget",
     extra=paste("--http-user=", user, " --http-passwd=", pw, sep="")
  )
  system(paste(system("which tar", intern=TRUE), "xzfv", tarFilename))
  untar(tarFilename, tar=system("which tar", intern=TRUE), exdir=binaryDir)
  registry <- scan(registryFilename, what="character", blank.lines.skip=FALSE, sep="\n", quiet=TRUE)
  lineHome <- grep("^HOME", registry)
  registry[lineHome] <- paste("HOME ", binaryDir, "/", tolower(corpus), sep="")
  lineInfo <- grep("^INFO", registry)
  registry[lineInfo] <- paste("INFO ", binaryDir, "/", tolower(corpus), "/.info", sep="")
  cat(registry, file=registryFilename, sep="\n")
  cat('Corpus "', corpus, '" has been installed\n', sep="")
  cat("registry at:", registryFilename, "\n")
  cat("corpus binaries at:", binaryDir, "\n\n")
  remove <- readline(paste("Remove", tarFilename, " (yes/no) ? \n"))
  if (grepl(".*yes.*", remove)==TRUE) {
    system(paste("rm", tarFilename))
  }
  cat("** enjoy the mining and the drilling! **\n")
  
}