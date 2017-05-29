#' Tools for CQP queries.
#' 
#' Test whether a character string is a CQP query, or turn a character
#' vector into CQP queries.
#' 
#' The \code{is.cqp} function guesses whether \code{query} is a CQP query 
#' and returns the respective logical value (TRUE/FALSE).
#' 
#' The \code{as.cqp} function takes a character vector as input and converts it
#' to a CQP query by putting the individual strings in quotation marks.
#' 
#' @param query character vector with at least one query
#' @name cqp
#' @references CQP Query Language Tutorial (\url{http://cwb.sourceforge.net/files/CQP_Tutorial.pdf})
#' @rdname cqp
#' @export is.cqp
#' @examples 
#' is.cqp("migration") # will return FALSE
#' is.cqp('"migration"') # will return TRUE
#' is.cqp('[pos = "ADJA"] "migration"') # will return TRUE
#' 
#' as.cqp("migration")
#' as.cqp(c("migration", "diversity"))
#' as.cqp(c("migration", "diversity"), collapse = TRUE)
#' as.cqp("migration", normalise.case = TRUE)
#' @return \code{is.cqp} returns a logical value, \code{as.cqp} a character vector
is.cqp <- function(query){
  isQuery <- unique(grepl('["|\']', query))
  if (length(isQuery) == 2){
    stop("Test whether query is a CQP query (or not) does not result in an unambigious result. Please check queries and/or state logical value explicitly.")
  }
  isQuery
} 

#' @param normalise.case logical
#' @param collapse logical, whether to collapse the queries into one
#' @export as.cqp
#' @rdname cqp
#' @name as.cqp
as.cqp <- function(query, normalise.case = FALSE, collapse = FALSE){
  if (is.logical(normalise.case) == FALSE) stop("normalise.case needs to be a logical value")
  if (is.logical(collapse) == FALSE) stop("collapse needs to be a logical value")
  cqp <- sapply(
    query,
    function(x){
      query <- gsub("\\s+", " ", x)
      cqpRaw <- lapply(
        unlist(strsplit(query, "\\s")),
        function(q){
          if ((substr(q, 1, 1) == '[') && (substr(q, nchar(q), nchar(q)) == ']')){
            retval <- q
          } else {
            retval <- paste('"', q, '"', sep='')
            if (normalise.case == TRUE) retval <- paste(retval, "%c", sep = " ")
          }
          retval
        })
      paste(cqpRaw, collapse = " ")
    })
  if (length(cqp) > 1 && collapse == TRUE){
    cqp <- paste('(', paste(cqp, sep='', collapse='|'), ')', sep="")
  }    
  cqp
}

#' Get ID for token.
#' 
#' Helper function for context method. Get ids for tokens.
#' 
#' @param corpus the CWB corpus to use
#' @param pAttribute the p-attribute to use
#' @param token character tokens to turn into ids (character vector length >= 1)
#' @param regex logical
#' @noRd
.token2id <- function(corpus, pAttribute, token = NULL, regex = FALSE){
  stopifnot(
    corpus %in% CQI$list_corpora(),
    pAttribute %in% CQI$attributes(corpus, type = "p")
    )
  if (is.null(token)) return( NULL )
  if (is.numeric(token)){
    return( token ) # do nothing if token is already numeric (i.e. presumably integer)
  } else {
    if (regex){
      retval <- unlist(lapply(
        token,
        function(x) CQI$regex2id(corpus = corpus, pAttribute = pAttribute, regex = x))
      )
    } else {
      retval <- CQI$str2id(corpus = corpus, pAttribute = pAttribute, str = token)
    }
    return( retval )
  }
}


.umlaute2punkt <- function(cmd){
  return(gsub('[\u00e4\u00f6\u00fc\u00df\u00c4\u00d6\u00dc]','.', cmd))
}

#' @export punctuation
#' @rdname trim-method
punctuation <- c(".", ",", ";", ":", "!", "?", "-", "--", "(", ")", "[", "]", "/")



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


# used by features,cooccurrences-method
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


#' Get objects of a certain class.
#'
#' @param class character, class to be looked for
#' @param envir character string, namespace to be searched
#' @return a list with the partitions found in the namespace
#' @export getObjects
getObjects <- function(class, envir = .GlobalEnv) {
  rawList <- sapply(ls(envir), function(x) class(get(x, envir = envir))[1])
  availableObjectsList <- rawList[rawList %in% class]
  names(unlist(availableObjectsList))
}

#' Get slot from object.
#' 
#' Auxiliary function to unify access to slots of S4 or R6 object.
#' 
#' @param x object to get slot from
#' @param name name of the slot
#' @export getSlot
getSlot <- function(x, name){
  if ("R6" %in% class(x)){
    return(x[[name]])
  } else {
    return(slot(x, name))
  }
}


.verboseOutput <- function(message, verbose = TRUE){
  if (verbose == TRUE){
    message(paste("...", message))
  } else if (verbose == "shiny"){
    shiny::incProgress(amount = 1, detail = message)
  }
}