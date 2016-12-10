setGeneric("encode", function(.Object, ...) standardGeneric("encode"))

#' Encode CWB Corpus.
#' 
#' @param .Object a data.frame to encode
#' @param name name of the (new) CWB corpus
#' @param pAttributes columns of .Object with tokens (such as word/pos/lemma)
#' @param sAttributes columns of .Object that will be encoded as structural attributes
#' @param registry path to the corpus registry
#' @param indexedCorpusDir directory where to create directory for indexed corpus files
#' @param verbose logical, whether to be verbose
#' @examples 
#' \dontrun{
#' library(tm)
#' library(tibble)
#' library(tidytext)
#' library(plyr)
#' reut21578 <- system.file("texts", "crude", package = "tm")
#' reuters.tm <- VCorpus(DirSource(reut21578), list(reader = readReut21578XMLasPlain))
#' reuters.tibble <- tidy(reuters.tm)
#' # reuters.tibble[["topics_cat"]] <- sapply(
#'   reuters.tibble[["topics_cat"]],
#'   function(x) paste(x, collapse = "|")
#' )
#' reuters.tibble[["places"]] <- sapply(
#'  reuters.tibble[["places"]],
#'  function(x) paste(x, collapse = "|")
#' )
#' reuters.tidy <- unnest_tokens(
#'   reuters.tibble, output = "word", input = "text", to_lower = FALSE
#'   )
#' encode(reuters.tidy, name = "reuters", sAttributes = c("language", "places"))
#' }
#' @rdname encode
setMethod("encode", "data.frame", function(
  .Object, name, pAttributes = "word", sAttributes = NULL,
  registry = Sys.getenv("CORPUS_REGISTRY"),
  indexedCorpusDir = NULL,
  verbose = TRUE
  ){
  
  if (require("plyr", quietly = TRUE)){
    # generate indexedCorpusDir if not provided explicitly
    if (is.null(indexedCorpusDir)){
      pathElements <- strsplit(registry, "/")[[1]]
      pathElements <- pathElements[which(pathElements != "")]
      pathElements <- pathElements[1:(length(pathElements) - 1)]
      registrySuperDir <- paste("/", paste(pathElements, collapse = "/"), sep = "")
      targetDir <- grep("index", list.files(registrySuperDir), value = TRUE)
      indexedCorpusDir <- file.path(registrySuperDir, targetDir, name)
      if (verbose){
        cat("No directory for indexed corpus provided - suggesting to use: ", indexedCorpusDir)
        feedback <- readline(prompt = "Y/N\n")
        if (feedback != "Y") return(NULL)
      }
    }
    
    if (!dir.exists(indexedCorpusDir)) dir.create(indexedCorpusDir)
    
    # starting basically - pAttribute 'word'
    
    if (verbose) message("... indexing s-attribute word")
    vrtTmpFile <- tempfile()
    cat(.Object[["word"]], file = vrtTmpFile, sep = "\n")
    
    cwbEncodeCmd <- paste(c(
      "cwb-encode",
      "-d", indexedCorpusDir, # directory with indexed corpus files
      "-f", vrtTmpFile, # vrt file
      "-R", file.path(registry, name) # filename of registry file
    ), collapse = " ")
    system(cwbEncodeCmd)
    
    # add other pAttributes than 'word'
    if (length(pAttributes > 1)){
      for (newAttribute in pAttributes[which(pAttributes != "word")]){
        if (verbose) message("... indexing p-attribute ", newAttribute)
        cwbEncodeCmd <- paste(c(
          "cwb-encode",
          "-d", indexedCorpusDir, # directory with indexed corpus files
          "-f", vrtTmpFile, # vrt file
          "-R", file.path(registry, name), # filename of registry file
          "-p", "-", "-P", newAttribute
        ), collapse = " ")
        system(cwbEncodeCmd)
      }
    }
    
    # call cwb-make
    if (verbose) message("... calling cwb-make")
    cwbMakeCmd <- paste(c("cwb-make", "-V", name), collapse = " ")
    system(cwbMakeCmd)
    
    .Object[["cpos"]] <- c(0:(nrow(.Object) - 1))
    
    # generate tab with begin and end corpus positions
    newTab <- plyr::ddply(
      .Object,
      .variables = "id",
      .fun = function(x){
        c(
          sapply(sAttributes, function(col) unique(x[[col]])[1]),
          cpos_left = x[["cpos"]][1],
          cpos_right = x[["cpos"]][length(x[["cpos"]])]
        )
      })
    
    for (sAttr in sAttributes){
      if (verbose) message("... indexing s-attribute ", sAttr)
      sAttributeTmpFile <- tempfile()
      outputMatrix <- matrix(
        data = c(
          as.character(newTab[["cpos_left"]]),
          as.character(newTab[["cpos_right"]]),
          as.character(newTab[[sAttr]])
        ),
        ncol = 3
      )
      outputVector <- paste(apply(outputMatrix, 1, function(x) paste(x, collapse = "\t")), collapse = "\n")
      outputVector <- paste(outputVector, "\n", sep = "")
      cat(outputVector, file = sAttributeTmpFile)
      cwbEncodeCmd <- paste(c(
        "cwb-s-encode",
        "-d",  indexedCorpusDir, # directory with indexed corpus files
        "-f", sAttributeTmpFile,
        "-V", paste("text", sAttr, sep = "_")
      ), collapse = " ")
      system(cwbEncodeCmd)
      if (verbose) message("... adding new s-attribute to registry file")
      regeditCmd <- paste(c("cwb-regedit", name, ":add", ":s", paste("text", sAttr, sep = "_")), collapse = " ")
      system(regeditCmd)
    } 
  } else {
    warning("package 'plyr' required but not available")
  }
  
})