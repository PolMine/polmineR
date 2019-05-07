#' Renamed Functions
#' 
#' These functions have been renamed in order to have a consistent coding style
#' that follows the snake_case convention. The "old" function still work to
#' maintain backwards compatiblity.
#' 
#' @name renamed
#' @rdname renamed
#' @param ... argument that are passed to the renamed function
NULL

#' @export sAttributes
#' @rdname renamed
sAttributes <- function(...) s_attributes(...)

#' @export pAttributes
#' @rdname renamed
pAttributes <- function(...) p_attributes(...)

#' @export getTokenStream
#' @rdname renamed
getTokenStream <- function(...) get_token_stream(...)

#' @export getTerms
#' @rdname renamed
getTerms <- function(...) terms(...)

#' @export getEncoding
#' @rdname renamed
getEncoding <- function(...) registry_get_encoding(...)

#' @export partitionBundle
#' @rdname renamed
partitionBundle <- function(...) partition_bundle(...)

#' @export as.partitionBundle
#' @rdname renamed
as.partitionBundle <- function(...) as.partition_bundle(...)

#' @export setTemplate
#' @rdname renamed
setTemplate <- function(...) set_template(...)

#' @export getTemplate
#' @rdname renamed
getTemplate <- function(...) get_template(...)

#' @rdname renamed
setMethod("corpus", "textstat", function(.Object){
  .Defunct(new = "get_corpus", package = "polmineR")
  get_corpus(.Object)
})

#' @rdname renamed
setMethod("corpus", "bundle", function(.Object){
  .Defunct(new = "get_corpus", package = "polmineR")
  get_corpus(.Object)
})


#' @rdname renamed
#' @param .Object A \code{kwic} object.
setMethod("corpus", "kwic", function(.Object){
  .Defunct(new = "get_corpus", package = "polmineR")
  get_corpus(.Object)
})


#' @title Defunct methods and functions.
#' @description Methods and functions not in use any more or that have been
#'   superseded by renamed functions.
#' 
#' @param ... Any arguments that may be passed into the defunct function/method.
#' @export browse
#' @rdname polmineR-defunct
#' @name polmineR-defunct
browse <- function(...) .Defunct(new = "html", package = "polmineR")


setOldClass("Corpus")

#' @details The RefClass `Corpus` has been superseded by the S4 `corpus`-class
#'   and adherent methods.
#' @rdname polmineR-defunct
#' @export Corpus
Corpus <- R6Class(
  
  "Corpus",
  
  public = list(
    
    corpus = NULL,
    registryDir = NULL,
    dataDir = NULL,
    encoding = NULL,
    cpos = NULL,
    p_attribute = NULL,
    s_attributes = NULL,
    size = NULL,
    stat = data.table(),
    
    initialize = function(corpus, p_attribute = NULL, s_attributes = NULL){
      
      stopifnot(is.character(corpus), length(corpus) == 1)
      if (!corpus %in% polmineR::corpus()[["corpus"]]) warning("corpus may not be available")
      self$corpus <- corpus
      
      self$registryDir <- Sys.getenv("CORPUS_REGISTRY")
      self$dataDir <- registry_get_home(corpus)
      self$encoding <- registry_get_encoding(corpus)
      self$size <- size(corpus)
      
      if (!is.null(p_attribute)){
        stopifnot(p_attribute %in% p_attributes(corpus))
        self$count(p_attribute)
      }
      
      if (!is.null(s_attributes)){
        dts <- lapply(
          s_attributes,
          function(s_attr){
            dt <- RcppCWB::s_attribute_decode(
              corpus = corpus,
              data_dir = registry_get_home(corpus),
              s_attribute = s_attr,
              encoding = registry_get_encoding(corpus),
              method = "R"
            )
            setkeyv(as.data.table(dt), c("cpos_left", "cpos_right"))
          }
        )
        dt <- dts[[1L]]
        setnames(dt, old = "value", new = s_attributes[1])
        if (length(s_attributes) > 1L){
          for (i in 2L:(length(s_attributes))){
            dt <- dt[ dts[[i]] ]
            setnames(dt, old = "value", new = s_attributes[i])
          }
        }
        
        self$cpos <- as.matrix(dt[, 1:2])
        self$s_attributes <- dt[, 3:ncol(dt)]
        self$s_attributes[["struc"]] <- 0:(nrow(self$s_attributes) - 1)
        setcolorder(self$s_attributes, neworder = c(ncol(self$s_attributes), 1:(ncol(self$s_attributes) - 1)))
      }
      
      invisible(self)
    },
    
    getRegions = function(s_attribute){
      rngFile <- file.path(self$dataDir, paste(s_attribute, "avx", sep = "."))
      rngFileSize <- file.info(rngFile)$size
      rngFileCon <- file(description = rngFile, open = "rb")
      rng <- readBin(con = rngFileCon, what = integer(), size = 4L, n = rngFileSize, endian = "big")
      close(rngFileCon)
      dt <- data.table(matrix(rng, ncol = 2, byrow = TRUE))
      colnames(dt) <- c("cpos", "offset")
      y <- dt[,{list(cpos_left = .SD[["cpos"]][1], cpos_right = .SD[["cpos"]][nrow(.SD)])}, by = offset]
      y[, offset := NULL][, struc := seq.int(from = 0, to = nrow(y) - 1)]
      y
    },
    
    getStructuralAttributes = function(s_attribute){
      avsFile <- file.path(self$dataDir, paste(s_attribute, "avs", sep = "."))
      avxFile <- file.path(self$dataDir, paste(s_attribute, "avx", sep = "."))
      avs <- readBin(con = avsFile, what = character(), n = file.info(avsFile)$size) # n needs to be estimated
      avx <- readBin(con = avxFile, what = integer(), size = 4L, n = file.info(avxFile)$size, endian = "big")
      offset <- avx[seq.int(from = 2, to = length(avx), by = 2)]
      levels <- sort(unique.default(offset))
      rank <- match(offset, levels)
      avs[rank]
    },
    
    count2 = function(p_attribute){
      
      cntFile <- file.path(self$dataDir, paste(p_attribute, "corpus.cnt", sep = "."))
      cntFileSize <- file.info(cntFile)$size
      cntFileCon <- file(description = cntFile, open = "rb")
      dt <- data.table(
        count = readBin(con = cntFileCon, what = integer(), size = 4L, n = cntFileSize, endian = "big")
      )
      close(cntFileCon)
      
      lexiconFile <- file.path(self$dataDir, paste(p_attribute, "lexicon", sep = "."))
      lexiconFileSize <- file.info(lexiconFile)$size
      lexiconFileCon <- file(description = lexiconFile, open = "rb")
      dt[[p_attribute]] <- readBin(con = cntFileCon, what = character(), n = cntFileSize)
      close(lexiconFileCon)
      dt
    },
    
    
    count = function(p_attribute = getOption("polmineR.p_attribute"), decode = TRUE){
      self$p_attribute <- p_attribute
      self$stat <- count(self$corpus, p_attribute = p_attribute, decode = decode)@stat
    },
    
    as.partition = function(){
      new(
        "partition",
        corpus = self$corpus,
        encoding = self$encoding,
        cpos = matrix(c(0, (size(self$corpus) - 1)), nrow = 1),
        stat = self$stat,
        size = self$size,
        p_attribute = if (is.null(self$p_attribute)) character() else self$p_attribute
      )
    },
    
    getInfo = function(as.html = FALSE){
      registry_get_info(self$corpus)
    },
    
    showInfo = function(){
      infoFile <- self$getInfo()
      if (file.exists(infoFile)){
        content <- readLines(infoFile)
        if (grepl(".md$", infoFile)){
          content <- markdown::markdownToHTML(text = content)
          content <-  htmltools::HTML(gsub("^.*<body>(.*?)</body>.*?$", "\\1", as.character(content)))
        } else {
          content <- htmltools::HTML(content)
        }
      } else {
        content <- htmltools::HTML("</br><i>corpus info file does not exist</i>")
      }
      if (interactive()) htmltools::html_print(content)
      invisible(content)
    }
  )
)





