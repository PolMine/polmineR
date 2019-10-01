#' Tools for CQP queries.
#' 
#' Test whether a character string is a CQP query, or turn a character
#' vector into CQP queries.
#' 
#' The \code{is.cqp} function guesses whether \code{query} is a CQP query 
#' and returns the respective logical value (\code{TRUE}/\code{FALSE}).
#' 
#' The \code{as.cqp} function takes a character vector as input and converts it
#' to a CQP query by putting the individual strings in quotation marks.
#' 
#' @param query A \code{character} vector with at least one CQP query.
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
#' @return \code{is.cqp} returns a logical value, \code{as.cqp} a character
#'   vector, \code{check_cqp_query} a logical value that is \code{TRUE} if all
#'   queries are valid, or \code{FALSE} if not.
is.cqp <- function(query){
  isQuery <- unique(grepl('["\']', query))
  if (length(isQuery) == 2)
    stop("Test whether query is a CQP query (or not) does not result in an unambigious result. Please check queries and/or state logical value explicitly.")
  isQuery
} 

#' @rdname cqp
#' @export check_cqp_query
#' @details The \code{check_cqp_query}-function will check that opening
#'   quotation marks are matched by closing quotation marks, to prevent crashes
#'   of CQP and the R session.
#' @examples
#' 
#' 
#' check_cqp_query('"Integration.*"') # TRUE, the query is ok
#' check_cqp_query('"Integration.*') # FALSE, closing quotation mark is missing
#' check_cqp_query("'Integration.*") # FALSE, closing quotation mark is missing
#' check_cqp_query(c("'Integration.*", '"Integration.*')) # FALSE too
check_cqp_query <- function(query){
  msg <- paste(c(
    "An issue occurred when checking query: %s\n",
    paste(
      "Number of quotation marks is not divisable by 2:",
      "Opening quotation marks are not matched by closing quotation marks, or vice versa.",
      "Aborting to avoid a potential crash of CQP and the entire R session.",
      "Please check query.", collapse = " "
    )
  ), collapse = "")
  
  check_results <- sapply(
    query,
    function(q){
      query_ok <- TRUE
      chars <- strsplit(q, split = "")[[1]]
      if (length(which(chars == '"')) %% 2 != 0) query_ok <- FALSE
      if (length(which(chars == "'")) %% 2 != 0) query_ok <- FALSE
      if (!query_ok) warning(sprintf(msg, q))
      query_ok
    }
  )
  any(check_results)
}


#' @param collapse A \code{logical} value, whether to collapse the queries into one.
#' @param normalise.case A \code{logical} value, if \code{TRUE}, a flag will be
#'   added to the query/queries to omit matching case.
#' @export as.cqp
#' @rdname cqp
#' @name as.cqp
as.cqp <- function(query, normalise.case = FALSE, collapse = FALSE){
  if (!is.logical(normalise.case)) stop("normalise.case needs to be a logical value")
  if (!is.logical(collapse)) stop("collapse needs to be a logical value")
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
#' @param p_attribute the p-attribute to use
#' @param token character tokens to turn into ids (character vector length >= 1)
#' @param regex logical
#' @noRd
.token2id <- function(corpus, p_attribute, token = NULL, regex = FALSE){
  stopifnot(
    corpus %in% .list_corpora(),
    p_attribute %in% registry_get_p_attributes(corpus)
    )
  if (is.null(token)) return( NULL )
  if (is.numeric(token)){
    return( token ) # do nothing if token is already numeric (i.e. presumably integer)
  } else {
    if (regex){
      retval <- unlist(lapply(
        token,
        function(x) cl_regex2id(corpus = corpus, p_attribute = p_attribute, regex = x, registry = registry())
      ))
    } else {
      retval <- cl_str2id(corpus = corpus, p_attribute = p_attribute, str = token, registry = registry())
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



#' \code{flatten} may be useful if you have a list of \code{partition_bundle} objects. This function will flatten the data
#' structure and return a \code{partition_bundle} object.
#' 
#' @return a \code{partition_bundle} object
#' @export flatten
#' @rdname partition_bundle-class
#' @name flatten
flatten <- function(object){
  retval <- new("partition_bundle")
  for (i in 1L:length(object)){
    if(!is.null(object[[i]])){
      if (length(object[[i]]@objects) > 0L){
        retval <- retval + object[[i]]
      }
    }
  } 
  retval
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
    result <- result[order(result$propability, decreasing = FALSE),]
    return(result)
  } else {
    
  }
  
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
#' @noRd
.get_objects <- function(class, envir = .GlobalEnv) {
  rawList <- sapply(ls(envir), function(x) class(get(x, envir = envir))[1])
  availableObjectsList <- rawList[rawList %in% class]
  names(unlist(availableObjectsList))
}


# used in shiny app
.get_slot <- function(x, name){
  if ("R6" %in% class(x)) x[[name]] else slot(x, name)
}


.message <- function(..., verbose = TRUE, type = "message", shiny = getOption("polmineR.shiny")){
  msg <- paste(unlist(list(...)), collapse = " ")
  
  # print(shiny)
  if (is.null(shiny)) shiny <- FALSE
  
  
  if (shiny){
    if (requireNamespace(package = "shiny", quietly = TRUE)){
      if (type %in% c("default", "message")){
        shiny::incProgress(amount = 1, detail = msg)
      } else {
        shiny::showNotification(msg, type = type)
      }
    }
  } else {
    if (type == "error"){
      stop(msg)
    } else if (type == "warning"){
      warning(msg)
    } else {
      if (verbose) message(paste("...", msg))
    }
  }
  
}


#' Round numeric columns of data.table
#' 
#' The object is modified in place, return value is NULL.
#' 
#' @param x A \code{data.table}.
#' @param digits Number of digits.
#' @noRd
round.data.table <- function(x, digits = 2L){
  column_classes <- sapply(x, function(column) is(column)[1])
  numeric_columns <- which(column_classes == "numeric")
  for (i in numeric_columns) x[, colnames(x)[i] := round(x[[i]], digits)]
  invisible( NULL )
}


.list_corpora = function() toupper(list.files( registry() ))

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
