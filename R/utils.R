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
.queryCpos <- function(query, Partition, pAttribute=drillingControls$pAttribute, verbose=TRUE) {
  if (length(query) > 1) warning("query needs to be a character vector with length 1")
  query <- .adjustEncoding(query, Partition@encoding)
  if (grepl('"', query) == FALSE) {
    pAttr <- paste(Partition@corpus, '.', pAttribute, sep='')
    cpos <- try(cqi_id2cpos(pAttr, cqi_str2id(pAttr, query)), silent=TRUE)
    if (is(cpos)[1] != "try-error"){
      hits <- as.matrix(data.frame(cpos, cpos))
    }
  } else if (grepl('"', query) == TRUE) {
    cqi_query(Partition@corpus, "Hits", query)
    cpos <- try(cqi_dump_subcorpus(paste(Partition@corpus, ":Hits", sep="")), silent=TRUE)
    if(is(cpos)[1] != "try-error"){
      hits <- matrix(cpos[,1:2], ncol=2)
    }
  }
  if (is(cpos)[1] != "try-error") {
    strucHits <- cqi_cpos2struc(
      paste(
        Partition@corpus, ".",
        names(Partition@sAttributes)[length(Partition@sAttributes)],
        sep=""),
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


#' get the values of a s-attribute in a corpus
#' 
#' The function is called by \code{partition}
#' 
#' @param corpus e.g. "PLPRBTTXT"
#' @param sattribute a structural attribute, e.g. "text_year"
#' @return returns a character vector
#' @author Andreas Blaette
#' @noRd
.sattribute2values <- function(corpus, sattribute){
  s <- paste(corpus, ".", sattribute, sep="")
  unique(cqi_struc2str(s, c(1:cqi_attribute_size(s))))
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

#' @param ns character string, namespace to be searched
#' @param class character, class to be looked for
#' @return a list with the partitions found in the namespace
#' @noRd
.getClassObjects <- function(ns, class) {
  rawList <- sapply(ls(ns), function(x) class(get(x, ns))[1])
  list <- rawList[rawList %in% class]
  sapply(names(list), function(x) get(x, ns), USE.NAMES=TRUE)
}

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
#' If you have a list of partitionClusters, this function will flatten the data
#' structure and return a partition Cluster object.
#' 
#' @param object a list (with partitionCluster objects)
#' @return a partitionCluster object
#' @export flatten
#' @rdname flatten
#' @name flatten
flatten <- function(object){
  newCluster <- new("partitionCluster")
  for (i in 1:length(object)){
    if(!is.null(object[[i]])){
      if (length(object[[i]]@partitions) > 0){
        newCluster <- newCluster + object[[i]]
      }
    }
  } 
  newCluster
}

.statisticalSummary <- function(object) {
  if (object@statisticalTest %in% c("ll", "chiSquare")){
    criticalValue <- c(3.84, 6.63, 7.88, 10.83)
    propability <- c(0.05, 0.01, 0.005, 0.001)
    no <- vapply(
      criticalValue,
      function(x) length(which(object@stat[[object@statisticalTest]]>x)),
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
  sAttributeDate <- cqi_attributes(corpus, 's')[grep('date', cqi_attributes(corpus, 's'))]
  sAttr <- paste(corpus, '.', sAttributeDate, sep='')
  allDatesInCorpus <- unique(cqi_struc2str(sAttr, c(0:(cqi_attribute_size(sAttr)-1))))
  daysSequence <- strftime(seq.dates(from=strftime(dateRange[1], format="%m/%d/%Y"), to=strftime(dateRange[2], format="%m/%d/%Y"), by="days"), format="%Y-%m-%d")
  daysInCorpus <- allDatesInCorpus[which(allDatesInCorpus %in% daysSequence)]
  daysInCorpus
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
#' @param collapse whether collapse the queries into one
#' @return a character vector
#' @export as.cqp
#' @rdname as.cqp
#' @name as.cqp
as.cqp <- function(queries, collapse=FALSE){
  cqp <- c()
  for (query in queries){
    bag <- c()
    for (q in unlist(strsplit(query, "\\s"))){
      if ((substr(q, 1, 1) !='[') && (substr(q, nchar(q), nchar(q)) != ']')){
        q <- paste('"', q, '"', sep='') 
      }
      bag <- append(bag, q)      
    }
    cqp <- append(cqp, paste('(', paste(bag, sep='', collapse=' '), ')', sep=""))
  }
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