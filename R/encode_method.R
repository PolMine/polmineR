#' @include regions_class.R

#' @rdname encode-method
setGeneric("encode", function(.Object, ...) standardGeneric("encode"))

#' Encode s-attribute or corpus.
#' 
#' If .Object is a \code{data.frame}, it needs to have a column with the token
#' stream (column name 'word'), and further columns with either p-attriutes,
#' or s-attributes. The corpus will be encoded successively, starting with the
#' p-attributes.
#' 
#' If .Object is a \code{data.table}, it is assumed to have three columns: The
#' left corpus position, the right corpus position and the value of a s-attribute
#' that will be encoded.
#' 
#' @param .Object a \code{data.frame} to encode
#' @param name name of the (new) CWB corpus
#' @param corpus the name of the CWB corpus
#' @param pAttributes columns of .Object with tokens (such as word/pos/lemma)
#' @param sAttribute a single s-attribute
#' @param sAttributes columns of .Object that will be encoded as structural attributes
#' @param registry path to the corpus registry
#' @param indexedCorpusDir directory where to create directory for indexed corpus files
#' @param verbose logical, whether to be verbose
#' @param ... further parameters
#' @rdname encode-method
#' @exportMethod encode
#' @examples
#' \donttest{
#' library(tm)
#' reut21578 <- system.file("texts", "crude", package = "tm")
#' reuters.tm <- VCorpus(DirSource(reut21578), list(reader = readReut21578XMLasPlain))
#' 
#' library(tidytext)
#' reuters.tibble <- tidy(reuters.tm)
#' reuters.tibble[["topics_cat"]] <- sapply(
#'   reuters.tibble[["topics_cat"]],
#'   function(x) paste(reuters.tibble[["topics_cat"]], collapse = "|")
#' )
#' reuters.tibble[["places"]] <- sapply(
#'   reuters.tibble[["places"]],
#'   function(x) paste(x, collapse = "|")
#' )
#' reuters.tidy <- unnest_tokens(
#'   reuters.tibble, output = "word", input = "text", to_lower = FALSE
#' )
#' encode(
#'   reuters.tidy, name = "reuters",
#'   sAttributes = c("language", "places")
#' )
#' }
setMethod("encode", "data.frame", function(
  .Object, name, pAttributes = "word", sAttributes = NULL,
  registry = Sys.getenv("CORPUS_REGISTRY"),
  indexedCorpusDir = NULL,
  verbose = TRUE
){
  
  if (!require("plyr", quietly = FALSE)){
    stop("package 'plyr' required but not available")
  }
  
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
  
  cwbEncodeCmd <- paste(
    c(
      "cwb-encode",
      "-d", indexedCorpusDir, # directory with indexed corpus files
      "-f", vrtTmpFile, # vrt file
      "-R", file.path(registry, name) # filename of registry file
    ),
    collapse = " "
  )
  system(cwbEncodeCmd)
  
  # add other pAttributes than 'word'
  if (length(pAttributes > 1)){
    for (newAttribute in pAttributes[which(pAttributes != "word")]){
      if (verbose) message("... indexing p-attribute ", newAttribute)
      cwbEncodeCmd <- paste(
        c(
          "cwb-encode",
          "-d", indexedCorpusDir, # directory with indexed corpus files
          "-f", vrtTmpFile, # vrt file
          "-R", file.path(registry, name), # filename of registry file
          "-p", "-", "-P", newAttribute
        ),
        collapse = " "
      )
      system(cwbEncodeCmd)
    }
  }
  
  # call cwb-make
  if (verbose) message("... calling cwb-make")
  cwbMakeCmd <- paste(c("cwb-make", "-V", name), collapse = " ")
  system(cwbMakeCmd)
  
  .Object[["cpos"]] <- 0:(nrow(.Object) - 1)
  
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
    if (verbose) message("... encoding s-attribute ", sAttr)
    dt <- data.table(
      cpos_left = as.character(newTab[["cpos_left"]]),
      cpos_right = as.character(newTab[["cpos_right"]]),
      value = as.character(newTab[[sAttr]])
      
    )
    encode(
      dt, corpus = toupper(name),
      sAttribute = paste("text", sAttr, sep = "_"),
      verbose = verbose
      )
  } 
})

#' @rdname encode-method
setMethod("encode", "data.table", function(.Object, corpus, sAttribute, verbose = TRUE){
  stopifnot(ncol(.Object) == 3)
  
  if (verbose) message("... preparing data")
  .Object[[1]] <- as.character(as.integer(.Object[[1]]))
  .Object[[2]] <- as.character(as.integer(.Object[[2]]))
  lines <- apply(.Object, 1, function(x) paste(x, collapse = "\t"))
  lines <- paste(lines, "\n", sep = "")
  tmp_file <- tempfile()
  cat(lines, file = tmp_file)
  
  if (verbose) message("... running cwb-s-encode")
  cmd <- c(
    "cwb-s-encode",
    "-d", RegistryFile$new(corpus)$getHome(),
    "-f", tmp_file,
    "-V", sAttribute
  )
  system(paste(cmd, collapse = " "))
  
  if (!sAttribute %in% sAttributes(corpus)){
    if (verbose) message("... adding sAttribute to registry")
    R <- RegistryFile$new(corpus)
    R$read()
    R$addSAttribute(sAttribute)
    R$write()
  }
})

#' @rdname encode-method
setMethod("encode", "regions", function(.Object, sAttribute, values, verbose = TRUE){
  dt <- as.data.table(.Object, values = values)
  encode(dt, corpus = .Object@corpus, sAttribute = sAttribute, verbose = verbose)
  invisible(dt)
})