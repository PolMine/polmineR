setOldClass("Corpus")


#' Corpus class.
#' 
#' The R6 \code{Corpus} class offers a set of methods to retrieve and manage CWB
#' indexed corpora.
#' 
#' @field corpus character vector (length 1), a CWB corpus
#' @field encoding encoding of the corpus (typically 'UTF-8' or 'latin1'), assigned
#' automatically upon initialization of the corpus
#' @field cpos a two-column \code{matrix} with regions of a corpus underlying the
#' s-attributes of the \code{data.table} in field \code{sAttributes}
#' @field sAttributes a \code{data.table} with the values of a set of sAttributes
#' @field stat a \code{data.table} with counts
#' 
#' @section Arguments:
#' \describe{
#'   \item{corpus}{a corpus}
#'   \item{registryDir}{the directory where the registry file resides}
#'   \item{dataDir}{the data directory of the corpus}
#'   \item{pAttribute}{p-attribute, to perform count}
#'   \item{sAttributes}{s-attributes}
#'   \item{decode}{logical, whether to turn token ids into strings upon counting}
#'   \item{as.html}{logical}
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(corpus, pAttribute = NULL, sAttributes = NULL)}}{Initialize a new object of class \code{Corpus}.}
#'   \item{\code{count(pAttribute = getOption("polmineR.pAttribute"), decode = TRUE)}}{Perform counts.}
#'   \item{\code{as.partition()}}{turn \code{Corpus} into a partition}
#'   \item{\code{getInfo(as.html = FALSE)}}{}
#'   \item{\code{showInfo()}}{}
#' }
#' 
#' @rdname Corpus-class
#' @export Corpus
#' @examples
#' use("polmineR")
#' REUTERS <- Corpus$new("REUTERS")
#' infofile <- REUTERS$getInfo()
#' if (interactive()) REUTERS$showInfo()
#' 
#' # use Corpus class to manage counts
#' REUTERS <- Corpus$new("REUTERS", pAttribute = "word")
#' REUTERS$stat
#' 
#' # use Corpus class for creating partitions
#' REUTERS <- Corpus$new("REUTERS", sAttributes = c("id", "places"))
#' usa <- partition(REUTERS, places = "usa")
#' sa <- partition(REUTERS, places = "saudi-arabia", regex = TRUE)
#' 
#' reut <- REUTERS$as.partition()
Corpus <- R6Class(
  
  "Corpus",
  
  public = list(
    
    corpus = NULL,
    registryDir = NULL,
    dataDir = NULL,
    encoding = NULL,
    cpos = NULL,
    pAttribute = NULL,
    sAttributes = NULL,
    size = NULL,
    stat = data.table(),
    
    initialize = function(corpus, pAttribute = NULL, sAttributes = NULL){
      
      stopifnot(is.character(corpus), length(corpus) == 1)
      if (!corpus %in% polmineR::corpus()[["corpus"]]) warning("corpus may not be available")
      self$corpus <- corpus
      
      self$registryDir <- Sys.getenv("CORPUS_REGISTRY")
      self$dataDir <- registry_get_home(corpus)
      self$encoding <- getEncoding(corpus)
      self$size <- size(corpus)
      
      if (!is.null(pAttribute)){
        stopifnot(pAttribute %in% pAttributes(corpus))
        self$count(pAttribute)
      }
      
      if (!is.null(sAttributes)){
        dt <- decode(corpus, sAttribute = sAttributes)
        self$cpos <- as.matrix(dt[, 1:2])
        self$sAttributes <- dt[, 3:ncol(dt)]
        self$sAttributes[["struc"]] <- 0:(nrow(self$sAttributes) - 1)
        setcolorder(self$sAttributes, neworder = c(ncol(self$sAttributes), 1:(ncol(self$sAttributes) - 1)))
      }
      
      invisible(self)
    },
    
    getRegions = function(sAttribute){
      rngFile <- file.path(self$dataDir, paste(sAttribute, "avx", sep = "."))
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
    
    getStructuralAttributes = function(sAttribute){
      avsFile <- file.path(self$dataDir, paste(sAttribute, "avs", sep = "."))
      avxFile <- file.path(self$dataDir, paste(sAttribute, "avx", sep = "."))
      avs <- readBin(con = avsFile, what = character(), n = file.info(avsFile)$size) # n needs to be estimated
      avx <- readBin(con = avxFile, what = integer(), size = 4L, n = file.info(avxFile)$size, endian = "big")
      offset <- avx[seq.int(from = 2, to = length(avx), by = 2)]
      levels <- sort(unique.default(offset))
      rank <- match(offset, levels)
      avs[rank]
    },
    
    count2 = function(pAttribute){
      
      cntFile <- file.path(self$dataDir, paste(pAttribute, "corpus.cnt", sep = "."))
      cntFileSize <- file.info(cntFile)$size
      cntFileCon <- file(description = cntFile, open = "rb")
      dt <- data.table(
        count = readBin(con = cntFileCon, what = integer(), size = 4L, n = cntFileSize, endian = "big")
      )
      close(cntFileCon)
      
      lexiconFile <- file.path(self$dataDir, paste(pAttribute, "lexicon", sep = "."))
      lexiconFileSize <- file.info(lexiconFile)$size
      lexiconFileCon <- file(description = lexiconFile, open = "rb")
      dt[[pAttribute]] <- readBin(con = cntFileCon, what = character(), n = cntFileSize)
      close(lexiconFileCon)
      
      dt

      # the idx file: offset positions
      # idxFile <- file.path(self$dataDir, "word.lexicon.idx")
      # idxFileSize <- file.info(idxFile)$size
      # idxFileCon <- file(description = idxFile, open = "rb")
      # idx <- readBin(con = idxFileCon, what = integer(), size = 4L, n = idxFileSize, endian = "big")
      # close(idxFileCon)
    },
    
    # summary = function(sAttributes = c(period = "text_lp", date = "text_date", dummy = "text_id")){
    #   # generate bundle, each of which will be evaluated in consecutive steps
    #   lpCluster <- partitionBundle(
    #     self$corpus,
    #     def = setNames(list(".*"), sAttributes["dummy"]),
    #     var = setNames(list(NULL), sAttributes["period"]),
    #     regex = TRUE, mc = FALSE
    #   )
    #   # extract data
    #   corpusData <- lapply(
    #     lpCluster@objects,
    #     function(x) {
    #       dates <- as.Date(sAttributes(x, sAttributes["date"]))
    #       c(
    #         first = as.character(min(dates, na.rm = TRUE)),
    #         last = as.character(max(dates, na.rm = TRUE)),
    #         no_token = size(x)
    #       )
    #     })
    #   
    #   # assemling data.frame
    #   tab <- data.frame(do.call(rbind, corpusData))
    #   tab[,"no_token"] <- as.integer(as.vector(tab[,"no_token"]))
    #   tab <- cbind(tab, avg_no_token = round(tab[,"no_token"]/tab[,"no"], digits = 0))
    #   
    #   tab_all <- data.frame(
    #     first = tab[1, "first"],
    #     last = tab[nrow(tab), "last"],
    #     no_token = sum(tab[, "no_token"]),
    #     avg_no_token = round(sum(tab[,"no_token"])/sum(tab[, "no"]), digits = 0),
    #     row.names = "ALL"
    #   )
    #   tab <- rbind(tab, tab_all)
    #   tab
    # },
    
    count = function(pAttribute = getOption("polmineR.pAttribute"), decode = TRUE){
      self$pAttribute <- pAttribute
      self$stat <- count(self$corpus, pAttribute = pAttribute, decode = decode)
    },
    
    # copy = function(registryDir, dataDir = NULL){
    #   indexedCorpusDirPkg
    #   lapply(
    #     list.files(indexedCorpusDirPkg, full.names = TRUE),
    #     function(x) file.copy(from=x, to=file.path(indexedCorporaDir, registryFile))
    #   )
    #   
    # },
    
    as.partition = function(){
      new(
        "partition",
        corpus = self$corpus,
        encoding = self$encoding,
        cpos = matrix(c(0, (size(self$corpus) - 1)), nrow = 1),
        stat = self$stat,
        size = self$size,
        pAttribute = if (is.null(self$pAttribute)) character() else self$pAttribute
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