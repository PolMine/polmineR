#' @include regions_class.R

#' @rdname encode-method
setGeneric("encode", function(.Object, ...) standardGeneric("encode"))

#' Encode s-attribute or corpus.
#' 
#' If \code{.Object} is a \code{data.frame}, it needs to have a column with the token
#' stream (column name 'word'), and further columns with either p-attributes,
#' or s-attributes. The corpus will be encoded successively, starting with the
#' p-attributes.
#' 
#' If \code{.Object} is a \code{data.table}, it is assumed to have three columns: The
#' left corpus position, the right corpus position and the value of a s-attribute
#' that will be encoded. The method is used to add s-attributes to a corpus.
#' 
#' If \code{.Object} is a (character) \code{vector}, there are two usages. If the corpus
#' defined by the parameter \code{corpus} does not yet exist, the vector is taken as
#' the word token stream. A new registry file, and a new data directory will be generated.  If
#' the \code{corpus} already exists, a new p-attribute will be added to the pre-existing
#' corpus.
#' 
#' @param .Object a \code{data.frame} to encode
#' @param name name of the (new) CWB corpus
#' @param corpus the name of the CWB corpus
#' @param pAttributes columns of .Object with tokens (such as word/pos/lemma)
#' @param pAttribute a single p-attribute
#' @param sAttribute a single s-attribute
#' @param sAttributes columns of .Object that will be encoded as structural attributes
#' @param registry path to the corpus registry
#' @param dataDir data directory for indexed corpus files
#' @param values values for regions
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
#'   reuters.tidy, name = "reuters2",
#'   sAttributes = c("language", "places")
#' )
#' }
setMethod("encode", "data.frame", function(
  .Object, name, pAttributes = "word", sAttributes = NULL,
  registry = Sys.getenv("CORPUS_REGISTRY"),
  dataDir = NULL,
  verbose = TRUE
){
  
  if (!require("plyr", quietly = FALSE)) stop("package 'plyr' required but not available")
  
  if (!"id" %in% colnames(.Object)) stop("column 'id' required")

  # starting basically - pAttribute 'word'
  encode(.Object[["word"]], corpus = name, dataDir = dataDir)

  # add other pAttributes than 'word'
  if (length(pAttributes > 1)){
    for (newAttribute in pAttributes[which(pAttributes != "word")]){
      encode(.Object[[newAttribute]], pAttribute = newAttribute)
    }
  }
  
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
  .Object[[1]] <- as.character(as.integer(.Object[[1]])) # ensure that cpos are integers
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

#' @rdname encode-method
setMethod("encode", "character", function(.Object, corpus, pAttribute = NULL, dataDir = NULL, verbose = TRUE){
  
  vrtTmpFile <- tempfile()
  
  if (corpus %in% corpus()[["corpus"]]){
    
    if (length(.Object) != size(corpus))
      stop("Length of character vector must be identical with size of corpus - not TRUE.")

    if (is.null(pAttribute))
      stop("pAttribute needs to be defined")
    
    if (pAttribute %in% pAttributes(corpus))
      stop("pAttribute already exists")
    
    if (any(grepl("^\\s*<.*?>\\s*$", .Object)))
      warning("there is markup in the character vector - cwb-encode will issue warnings")

    # ensure that encoding of .Object vector is encoding of corpus
    if (verbose) message("... checking encoding")
    if (!getEncoding(corpus) %in% names(table(Encoding(.Object)))){
      if (verbose) message("... encoding of vector different from corpus - assuming it to be that of the locale")
      .Object <- as.corpusEnc(.Object, from = localeToCharset()[1], corpusEnc = getEncoding(corpus))
    }

    if (verbose) message("... writing vector to disk for p-attribute ", pAttribute)
    cat(.Object, file = vrtTmpFile, sep = "\n")
    
    if (verbose) message("... calling cwb-encode")
    pAttrsOld <- RegistryFile$new(corpus)$getPAttributes()
    cwbEncodeCmdVec <- c(
      "cwb-encode",
      "-d", RegistryFile$new(corpus)$getHome(), # directory with indexed corpus files
      "-f", vrtTmpFile,
      "-R", file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(corpus)),
      "-p", "-", "-P", pAttribute
    )
    cwbEncodeCmd <- paste0(cwbEncodeCmdVec, collapse = " ")
    system(cwbEncodeCmd)
    
    # cwb-encode may drop attributes from registry file apart from the newly encoded one ... 
    missingAttrs <- pAttrsOld[!pAttrsOld %in% RegistryFile$new(corpus)$getPAttributes()]
    for (attr in missingAttrs) RegistryFile$new(corpus)$addPAttribute(attr)
    
  } else {
    
    if (verbose) message("Creating new CWB indexed corpus ", corpus)
    
    encodingInput <- unique(Encoding(.Object))
    if (length(encodingInput) == 1){
      
    } else if (length(encodingInput) == 2){
      
    } else {
      stop("please check encoding of the input character vector - more than one encoding found")
    }
    
    if (verbose) message("... writing token stream to disk")
    cat(.Object, file = vrtTmpFile, sep = "\n")
    
    if (verbose) message("... check for data directory")
    if (is.null(dataDir)){
      superDir <- dirname(Sys.getenv("CORPUS_REGISTRY"))
      targetDir <- grep("index", list.files(superDir), value = TRUE, perl = TRUE)
      if (length(targetDir) != 1) stop("no dataDir provided, no candidate found")
      dataDir <- file.path(superDir, targetDir, tolower(corpus))
      feedback <- readline(
        prompt = sprintf(
          "No directory for indexed corpus provided - suggesting to use: %s (Y/N) ",
          dataDir
        )
      )
      if (feedback != "Y") stop("aborting")
      if (!file.exists(dataDir)) dir.create(dataDir)
    }
    
    if (verbose) message("... running cwb-encode")
    cwbEncodeCmdVec <- c(
      "cwb-encode",
      "-d", dataDir, # directory with indexed corpus files
      "-f", vrtTmpFile,
      "-R", file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(corpus))
    )
    cwbEncodeCmd <- paste0(cwbEncodeCmdVec,collapse = " ")
    
    system(cwbEncodeCmd)
    
  }
  
  if (verbose) message("... running cwb-make")
  cwbMakeCmd <- paste0(
    c("cwb-make", "-V", corpus, "-r", Sys.getenv("CORPUS_REGISTRY")),
    collapse = " "
    )
  system(cwbMakeCmd)
  use(dir = Sys.getenv("CORPUS_REGISTRY"))
})