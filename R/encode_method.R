#' @include regions_class.R

#' @rdname encode-method
setGeneric("encode", function(.Object, ...) standardGeneric("encode"))

#' Encode s-attribute or corpus.
#' 
#' If \code{.Object} is a \code{data.frame}, it needs to have a column with the token
#' stream (column name 'word'), and further columns with either p-attributes,
#' or s-attributes. The corpus will be encoded successively, starting with the
#' p-attributes in the columns defined by \code{pAttributes}.
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
#' 
#' tmpCwbDir <- tempdir()
#' registryDir <- file.path(tmpCwbDir, "registry")
#' dir.create(registryDir)
#' dir.create(file.path(tmpCwbDir, "indexed_corpora"))
#' resetRegistry(registryDir = registryDir)
#' 
#' encode(reuters.tidy, name = "reuters", sAttributes = c("language", "places"))
#' corpus()
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
    .message("encoding s-attribute ", sAttr, verbose = verbose)
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
#' @importFrom data.table fwrite
setMethod("encode", "data.table", function(.Object, corpus, sAttribute, verbose = TRUE){
  stopifnot(ncol(.Object) == 3)
  
  .message("preparing data", verbose = verbose)
  .Object[[1]] <- as.character(as.integer(.Object[[1]])) # ensure that cpos are integers
  .Object[[2]] <- as.character(as.integer(.Object[[2]]))
  .Object[[3]] <- as.corpusEnc(.Object[[3]], corpusEnc = getEncoding(corpus))
    
  tmp_file <- tempfile()
  data.table::fwrite(x = .Object, file = tmp_file, quote = FALSE, sep = "\t", col.names = FALSE)

  .message("running cwb-s-encode", verbose = verbose)
  cmd <- c(
    "cwb-s-encode",
    "-d", RegistryFile$new(corpus)$getHome(),
    "-f", tmp_file,
    "-V", sAttribute
  )
  system(paste(cmd, collapse = " "))
  
  if (!sAttribute %in% sAttributes(corpus)){
    .message("adding sAttribute to registry", verbose = verbose)
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
    .message("checking encoding", verbose = verbose)
    if (!getEncoding(corpus) %in% names(table(Encoding(.Object)))){
      .message("encoding of vector different from corpus - assuming it to be that of the locale", verbose = verbose)
      .Object <- as.corpusEnc(.Object, from = localeToCharset()[1], corpusEnc = getEncoding(corpus))
    }

    .message("writing vector to disk for p-attribute ", pAttribute, verbose = verbose)
    cat(.Object, file = vrtTmpFile, sep = "\n")
    
    .message("calling cwb-encode", verbose = verbose)
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
    
    .message("Creating new CWB indexed corpus ", corpus, verbose = verbose)
    
    encodingInput <- unique(Encoding(.Object))
    if (length(encodingInput) == 1){
      
    } else if (length(encodingInput) == 2){
      
    } else {
      stop("please check encoding of the input character vector - more than one encoding found")
    }
    
    .message("writing token stream to disk", verbose = verbose)
    cat(.Object, file = vrtTmpFile, sep = "\n")
    
    .message("check for data directory", verbose = verbose)
    if (is.null(dataDir)){
      superDir <- dirname(Sys.getenv("CORPUS_REGISTRY"))
      targetDir <- grep("index", list.files(superDir), value = TRUE, perl = TRUE)
      if (length(targetDir) != 1) stop("no dataDir provided, no candidate found")
      dataDir <- file.path(superDir, targetDir, tolower(corpus))
      if (interactive()){
        feedback <- readline(
          prompt = sprintf(
            "No directory for indexed corpus provided - suggesting to use: %s (Y/N) ",
            dataDir
          )
        )
        if (feedback != "Y") stop("aborting")
      }
      if (!file.exists(dataDir)) dir.create(dataDir)
    }
    
    .message("running cwb-encode", verbose = verbose)
    cwbEncodeCmdVec <- c(
      "cwb-encode",
      "-d", dataDir, # directory with indexed corpus files
      "-f", vrtTmpFile,
      "-R", file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(corpus))
    )
    cwbEncodeCmd <- paste0(cwbEncodeCmdVec,collapse = " ")
    
    system(cwbEncodeCmd)
    
  }
  
  .message("running cwb-make", verbose = verbose)
  cwbMakeCmd <- paste0(
    c("cwb-make", "-V", corpus, "-r", Sys.getenv("CORPUS_REGISTRY")),
    collapse = " "
    )
  system(cwbMakeCmd)
  use(dir = Sys.getenv("CORPUS_REGISTRY"))
})