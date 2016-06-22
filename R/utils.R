#' Get the corpus positions for a query
#' 
#' Get the corpus positions for a query in a given partition
#' 
#' The function works for single words, which is the quick optin.
#' Also, you can use the full CQP syntax. In this case, make sure to
#' use single quotes, and double quotes for individual words
#' (see example below). Input needs to be in UTF-8
#' 
#' @param query character vector length 1 providing a query (encoding: UTF-8), see details
#' @param partition a partition object
#' @param pAttribute either 'word' or 'lemma', not used 
#' @return a data frame with two columns with cpos (start and end positions of hits)
#' @author Andreas Blaette
#' @example
#' \dontrun{
#' foo <- .queryCpos("Integration", partitionObject)
#' foo <- .queryCpos('"Menschen" "mit" "Migrationshintergrund"', partitionObject)
#' }
#' @noRd
.queryCpos <- function(query, Partition, pAttribute=getOption("polmineR.pAttribute"), verbose=TRUE) {
  if (length(query) > 1) warning("query needs to be a character vector with length 1")
  query <- .adjustEncoding(query, Partition@encoding)
  if (grepl('"', query) == FALSE) {
    pAttr <- paste(Partition@corpus, '.', pAttribute, sep='')
    cpos <- try(CQI$id2cpos(Partition@corpus, pAttribute, CQI$str2id(Partition@corpus, pAttribute, query)), silent=TRUE)
    if (is(cpos)[1] != "try-error"){
      hits <- as.matrix(data.frame(cpos, cpos))
    }
  } else if (grepl('"', query) == TRUE) {
    CQI$query(Partition@corpus, query)
    cpos <- try(CQI$dump_subcorpus(Partition@corpus))
    if(is(cpos)[1] != "try-error"){
      hits <- matrix(cpos[,1:2], ncol=2)
    }
  }
  if (is(cpos)[1] != "try-error") {
    strucHits <- CQI$cpos2struc(
      Partition@corpus, 
      names(Partition@sAttributes)[length(Partition@sAttributes)],
      hits[,1]
      )
    hits <- hits[which(strucHits %in% Partition@strucs),]
    if (is(hits)[1] == "integer") {
      hits <- matrix(hits, nrow=1)
    }
  } else {
    if (verbose == TRUE) warning("no hits for query -> ", query)
    hits = NULL
  }
  hits
}

#' The current time 
#' 
#' Helper function for ouput to estimate the time that functions require
#' @noRd
.takeoff <- function(){strsplit(date(), ' ')[[1]][5]}

#' bla
#'
#' is called by: 
#' @noRd
.restrictionsCmd <- function(sattributes) {
  sapply(
    names(sattributes),
    function(x) paste("?", x,"=/",
                      gsub('\\s','.', paste(sattributes[[x]], collapse='\\|')),
                      "/",sep=""))
}

.umlaute2punkt <- function(cmd){
  return(gsub('[\u00e4\u00f6\u00fc\u00df\u00c4\u00d6\u00dc]','.', cmd))
}

#' @export punctuation
#' @rdname trim-method
punctuation <- c(".", ",", ";", ":", "!", "?", "-", "--", "(", ")", "[", "]", "/")


#' adjust encoding
#' 
#' use case: encoding of console input is UTF-8, the corpus encoding is latin-1
#'
#' @param characterVector the character vector to be adjusted
#' @param partitionEncoding the encoding to be set
#' @return a character vector with adjusted encoding
#' @noRd
.adjustEncoding <- function(characterVector, partitionEncoding){
  retval <- sapply(
    as.list(characterVector),
    function(x) {
      enc <- Encoding(x)
      if ( enc != "unknown" && enc != partitionEncoding ) {
        x <- iconv(x, from=enc, to=partitionEncoding)
      }
      x
    }
    )
  return(retval)
}

#' flatten a nested list
#' 
#' If you have a list of partitionBundles, this function will flatten the data
#' structure and return a partition Bundle object.
#' 
#' @param object a list (with partitionBundle objects)
#' @return a partitionBundle object
#' @export flatten
#' @rdname flatten
#' @name flatten
flatten <- function(object){
  newBundle <- new("partitionBundle")
  for (i in 1:length(object)){
    if(!is.null(object[[i]])){
      if (length(object[[i]]@objects) > 0){
        newBundle <- newBundle + object[[i]]
      }
    }
  } 
  newBundle
}

.statisticalSummary <- function(object) {
  if (object@method %in% c("ll", "chiSquare")){
    criticalValue <- c(3.84, 6.63, 7.88, 10.83)
    propability <- c(0.05, 0.01, 0.005, 0.001)
    no <- vapply(
      criticalValue,
      function(x) length(which(object@stat[[object@method]]>x)),
      FUN.VALUE=1
    )
    result <- data.frame(propability, criticalValue, no)
    result <- result[order(result$propability, decreasing=FALSE),]
  }
  return(result)
}

.filter <- list(
  include=function(x,y) {x %in% y},
  exclude=function(x,y) {!(x %in% y)}
)

#' generate the sattribute
#' 
#' Helper function for partition
#' 
#' @param corpus the CWB corpus used
#' @param dateRange a character with two character strings: the start date, and the end date
#' @return a character vector (length > 1) that can be used in sAttribute definition
#' @author Andreas Blaette
#' @export datesPeriod
datesPeriod <- function(corpus, dateRange) {
  if (requireNamespace("chron", quietly=TRUE)){
    sAttributeDate <- CQI$attributes(corpus, 's')[grep('date', CQI$attributes(corpus, 's'))]
    # sAttr <- paste(corpus, '.', sAttributeDate, sep='')
    allDatesInCorpus <- unique(CQI$struc2str(corpus, sAttributeDate, c(0:(CQI$attribute_size(corpus, sAttributeDate)-1))))
    daysSequence <- strftime(chron::seq.dates(from=strftime(dateRange[1], format="%m/%d/%Y"), to=strftime(dateRange[2], format="%m/%d/%Y"), by="days"), format="%Y-%m-%d")
    daysInCorpus <- allDatesInCorpus[which(allDatesInCorpus %in% daysSequence)]
    retval <- daysInCorpus    
  } else {
    warning("the 'chron'-package needs to be installed but is not available")
    stop()
  }
  retval
}



.getCorpusEncoding <- function(corpus){
  registry <- scan(
    file=file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(corpus)),
    sep="\n",
    what="character",
    quiet=TRUE
  )
  encodingLine <- registry[grep('charset\\s*=\\s*"', registry)]
  encoding <- sub('^.*charset\\s*=\\s*"(.+?)".*$', "\\1", encodingLine)
  encoding <- toupper(encoding)
  if (!encoding %in% iconvlist()){
    warning('Please check encoding in the registry file (charset="..." provides unknown encoding) or provide encoding explicitly')
  }
  return(tolower(encoding))
}

#' Convert a string to a CQP query
#' 
#' Takes a simple string as an imput and converts it to a valid CQP query
#' @param queries a character vector
#' @param normalise_case logical
#' @param collapse whether collapse the queries into one
#' @return a character vector
#' @export as.cqp
#' @rdname as.cqp
#' @name as.cqp
as.cqp <- function(queries, normalise_case=FALSE, collapse=FALSE){
  cqp <- sapply(
    queries,
    function(query){
      query <- gsub("\\s+", " ", query)
      cqpRaw <- lapply(
        unlist(strsplit(query, "\\s")),
        function(q){
          if ((substr(q, 1, 1) == '[') && (substr(q, nchar(q), nchar(q)) == ']')){
            retval <- q
          } else {
            retval <- paste('"', q, '"', sep='')
            if (normalise_case == TRUE) retval <- paste(retval, "%c", sep=" ")
          }
          retval
        })
      paste(cqpRaw, collapse=" ")
      })
  if (length(cqp)>1 && collapse==TRUE){
    cqp <- paste('(', paste(cqp, sep='', collapse='|'), ')', sep="")
  }    
  return(cqp)
}

#' @export .importPolMineCorpus
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



.progressBar <- function(i, total, barLength=80) {
  no <- floor(barLength * (i/total))
  if (i>1) cat(paste(rep("\b", times=barLength+2+8), collapse=""))
  cat(paste("0% [", paste(rep("=", times=no), collapse=""), paste(rep(" ", times=barLength-no), collapse=""),  "] 100%", sep=""))
  if (i == total) cat("\n")
}


# used by compare,cooccurrences-method
# listed here because it may be used by other methods
.minMaxId <- function(row){
  if (row["nodeId"] == row["cooccurrenceId"]){
    retval <- c(row, idMin=row["nodeId"], idMax=row["cooccurrenceId"])
  } else {
    idMin <- min(row["nodeId"], row["cooccurrenceId"])
    idMax <- max(row["nodeId"], row["cooccurrenceId"])
    retval <- c(row, idMin=idMin, idMax=idMax)
  }
  return(retval)
}


# exactly the same function is included in polmineR.shiny
#' @param ns character string, namespace to be searched
#' @param class character, class to be looked for
#' @return a list with the partitions found in the namespace
#' @noRd
.getClassObjectsAvailable <- function(ns, class) {
  rawList <- sapply(ls(ns), function(x) class(get(x, ns))[1])
  availableObjectsList <- rawList[rawList %in% class]
  names(unlist(availableObjectsList))
}

# used by .crosstab
.mapMatrices <- function(matrixToMatch, matrixToAdjust){
  colnames(matrixToAdjust)[which(colnames(matrixToAdjust)=="")] <- 'NA'
  rownames(matrixToAdjust)[which(rownames(matrixToAdjust)=="")] <- 'NA'
  colnames(matrixToMatch)[which(colnames(matrixToMatch)=="")] <- 'NA'
  rownames(matrixToMatch)[which(rownames(matrixToMatch)=="")] <- 'NA'
  missingRows <- rownames(matrixToMatch)[!rownames(matrixToMatch) %in% rownames(matrixToAdjust)]
  if (length(missingRows) > 0){
    matrixToAppend <- matrix(
      data=0, nrow=length(missingRows), ncol=ncol(matrixToAdjust),
      dimnames=list(missingRows, colnames(matrixToAdjust))
    )
    matrixToAdjust <- rbind(matrixToAdjust, matrixToAppend)
  }
  matrixToMatchOrdered <- matrixToMatch[order(rownames(matrixToMatch)),]
  matrixToAdjustOrdered <- matrixToAdjust[order(rownames(matrixToAdjust)),]
  matrixFinal <- matrix(
    data=0, nrow=nrow(matrixToMatchOrdered), ncol=ncol(matrixToMatchOrdered),
    dimnames=dimnames(matrixToMatchOrdered)
  )
  for (colName in colnames(matrixToAdjustOrdered)){
    matrixFinal[,colName] <- matrixToAdjustOrdered[,colName] 
  }
  matrixFinal
}

.splitMatrixIntoEquallySizedParts <- function(x, n){
  chunkFactor <- cut(
    c(1:nrow(x)),
    unique(c(1, floor(c(1:(n-1))*(nrow(x)/n)), nrow(x))),
    include.lowest=TRUE
  )
  chunkedMatrix <- split(x, chunkFactor)
  lapply(chunkedMatrix, function(m) matrix(m, ncol=ncol(x)))
}


.verboseOutput <- function(message, verbose){
  if (verbose == TRUE){
    message(paste("...", message))
  } else if (verbose == "shiny"){
    shiny::incProgress(amount = 1, detail = message)
  }
}