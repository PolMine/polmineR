#' @include partition.R bundle.R S4classes.R
NULL

#' Get corpus/corpora available or used.
#' 
#' Calling \code{corpus()} will return a \code{data.frame} listing the corpora
#' described in the active registry directory, and some basic information on the
#' corpora. If \code{.Object} is an object inheriting from the \code{textstat},
#' or the \code{bundle} class, the corpus used to generate the object is
#' returned.
#' @param .Object An object inheriting from the \code{textstat} or \code{bundle}
#'   superclasses.
#' @param server The name of an Open CPU server (can be an IP address) that
#'   hosts corpora.
#' @param ... Placeholder in the definition of the generic method for defining
#'   further arguments in method definitions.
#' @exportMethod corpus
#' @rdname corpus-method
#' @examples
#' use("polmineR")
#' corpus()
#' 
#' p <- partition("REUTERS", places = "kuwait")
#' corpus(p)
#' 
#' pb <- partition_bundle("REUTERS", s_attribute = "id")
#' corpus(pb)
setGeneric("corpus", function(.Object, ...) standardGeneric("corpus"))


#' @rdname corpus-method
setMethod("corpus", "textstat", function(.Object) .Object@corpus)

#' @rdname corpus-method
setMethod("corpus", "kwic", function(.Object) .Object@corpus)

#' @rdname corpus-method
setMethod("corpus", "character", function(.Object, server = NULL){
  if (is.null(server)){
    properties <- registry_get_properties(.Object)
    y <- new(
      "corpus",
      corpus = .Object,
      encoding = registry_get_encoding(.Object),
      data_dir = registry_get_home(.Object),
      type = if ("type" %in% names(properties)) properties[["type"]] else character()
    )
    return(y)
  } else {
    y <- ocpu_exec(fn = "corpus", server = server, .Object = .Object)
    y <- as(y, "remote_corpus")
    y@server <- server
    return(y)
  }
})

#' @rdname corpus-method
setMethod("corpus", "bundle", function(.Object){
  unique(sapply(.Object@objects, function(x) x@corpus))
})

#' @rdname corpus-method
setMethod("corpus", "missing", function(){
  if (nchar(Sys.getenv("CORPUS_REGISTRY")) > 1){
    corpora <- .list_corpora()
    y <- data.frame(
      corpus = corpora,
      size = unname(sapply(corpora,function(x) cl_attribute_size(corpus = x, attribute = registry_get_p_attributes(x)[1], attribute_type = "p", registry = registry()))),
      template = unname(sapply(corpora, function(x) x %in% names(getOption("polmineR.templates")))),
      stringsAsFactors = FALSE
    )
  } else {
    y <- data.frame(corpus = character(), size = integer())
  }
  return(y)
})

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
#' s-attributes of the \code{data.table} in field \code{s_attributes}
#' @field s_attributes a \code{data.table} with the values of a set of s-attributes
#' @field stat a \code{data.table} with counts
#' 
#' @section Arguments:
#' \describe{
#'   \item{corpus}{a corpus}
#'   \item{registryDir}{the directory where the registry file resides}
#'   \item{dataDir}{the data directory of the corpus}
#'   \item{p_attribute}{p-attribute, to perform count}
#'   \item{s_attributes}{s-attributes}
#'   \item{decode}{logical, whether to turn token ids into strings upon counting}
#'   \item{as.html}{logical}
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(corpus, p_attribute = NULL, s_attributes = NULL)}}{Initialize a new object of class \code{Corpus}.}
#'   \item{\code{count(p_attribute = getOption("polmineR.p_attribute"), decode = TRUE)}}{Perform counts.}
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
#' REUTERS <- Corpus$new("REUTERS", p_attribute = "word")
#' REUTERS$stat
#' 
#' # use Corpus class for creating partitions
#' REUTERS <- Corpus$new("REUTERS", s_attributes = c("id", "places"))
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
      
      # the idx file: offset positions
      # idxFile <- file.path(self$dataDir, "word.lexicon.idx")
      # idxFileSize <- file.info(idxFile)$size
      # idxFileCon <- file(description = idxFile, open = "rb")
      # idx <- readBin(con = idxFileCon, what = integer(), size = 4L, n = idxFileSize, endian = "big")
      # close(idxFileCon)
    },
    
    # summary = function(s_attributes = c(period = "text_lp", date = "text_date", dummy = "text_id")){
    #   # generate bundle, each of which will be evaluated in consecutive steps
    #   lpCluster <- partition_bundle(
    #     self$corpus,
    #     def = setNames(list(".*"), s_attributes["dummy"]),
    #     var = setNames(list(NULL), s_attributes["period"]),
    #     regex = TRUE, mc = FALSE
    #   )
    #   # extract data
    #   corpusData <- lapply(
    #     lpCluster@objects,
    #     function(x) {
    #       dates <- as.Date(s_attributes(x, s_attributes["date"]))
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
    
    count = function(p_attribute = getOption("polmineR.p_attribute"), decode = TRUE){
      self$p_attribute <- p_attribute
      self$stat <- count(self$corpus, p_attribute = p_attribute, decode = decode)@stat
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



#' @noRd
.s_attributes_stop_if_nested <- function(corpus, s_attr){
  max_attr <- unique(sapply(s_attr, function(s) cl_attribute_size(corpus = corpus, attribute = s, attribute_type = "s", registry = registry())))
  if (length(max_attr) != 1){
    stop(
      sprintf("Differing attribute size of s-attributes detected (%s), ", paste(s_attr, collapse = "/")),
      "but the method does not (yet) work for nested XML / nested structural attributes."
    )
  }
  max_attr
}

#' @examples
#' use("polmineR")
#' a <- corpus("GERMAPARLMINI")
#' sc <- subset(a, speaker == "Angela Dorothea Merkel")
#' sc <- subset(a, speaker == "Bärbel Höhn")
#' sc <- subset(a, speaker == "Angela Dorothea Merkel" & date == "2009-10-28")
#' sc <- subset(a, grepl("Merkel", speaker))
#' sc <- subset(a, grepl("Merkel", speaker) & date == "2009-10-28")
#' 
#' sc <- subset(a, speaker = "Angela Dorothea Merkel")
#' sc <- subset(a, speaker = "Bärbel Höhn")
#' sc <- subset(a, speaker = "Angela Dorothea Merkel", date = "2009-10-28")
#' sc <- subset(a, speaker = "Merkel", regex = TRUE)
#' sc <- subset(a, speaker = c("Merkel", "Kauder"), regex = TRUE)
#' sc <- subset(a, speaker = "Merkel", date = "2009-10-28", regex = TRUE)
#' 
#' who <- "Volker Kauder"
#' subset(a, quote(speaker == who))
#' 
#' for (who in c("Angela Dorothea Merkel", "Volker Kauder", "Ronald Pofalla")){
#'    sc <- subset(a, bquote(speaker == .(who)))
#'    print(size(sc))
#' }
#' 
#' b <- lapply(
#'   c("Angela Dorothea Merkel", "Volker Kauder", "Ronald Pofalla"),
#'   function(who) subset(a, bquote(speaker == .(who)))
#' )
#' sapply(b, size)
#' @rdname subcorpus-class
#' @param subset A \code{logical} expression indicating elements or rows to
#'   keep. The expression may be unevaluated (using \code{quote} or
#'   \code{bquote}).
#' @importFrom data.table setindexv
#' @param regex A \code{logical} value. If \code{TRUE}, values for s-attributes
#'   defined using the three dots (...) are interpreted as regular expressions
#'   and passed into a \code{grep} call for subsetting a table with the regions
#'   and values of structural attributes. If \code{FALSE} (the default), values
#'   for s-attributes must match exactly.
setMethod("subset", "corpus", function(x, subset, regex = FALSE, ...){
  stopifnot(is.logical(regex))
  s_attr <- character()
  
  if (!missing(subset)){
    expr <- substitute(subset)
    # The expression may also have been passed in as an unevaluated expression. In
    # this case, it is "unwrapped". Note that parent.frames looks back two generations
    # because the S4 Method inserts an additional layer to the original calling
    # environment
    if (class(try(eval(expr, envir = parent.frame(n = 1L:2L)), silent = TRUE)) == "call"){
      expr <- eval(expr, envir = parent.frame(n = 1L:2L))
    }
    # Adjust the encoding of the expression to the one of the corpus. Adjusting
    # encodings is expensive, so the (small) epression will be adjusted to the
    # encoding of the corpus, not vice versa
    if (localeToCharset()[1] != x@encoding)
      expr <- .recode_call(x = expr, from = localeToCharset()[1], to = x@encoding)
    s_attr_expr <- s_attributes(expr, corpus = x) # get s_attributes present in the expression
    s_attr <- c(s_attr, s_attr_expr)
  }

  dots <- list(...)
  if (length(dots) > 0L){
    if (!all(names(dots) %in% s_attributes(x))){
      stop("Aborting - at least one of the s-attributes provided as an argument is not available.")
    }
    s_attr_dots <- names(dots)
    s_attr <- c(s_attr, s_attr_dots)
    if (localeToCharset()[1] != x@encoding){
      s_attr_dots <- lapply(s_attr_dots, function(v) as.corpusEnc(v, corpusEnc = x@encoding))
    }
  }
  
  # Reading the binary file with the ranges for the whole corpus is faster than using
  # the RcppCWB functionality. The assumption here is that the XML is flat, i.e. no need
  # to read in seperate rng files.
  rng_file <- file.path(x@data_dir, paste(s_attr[1], "rng", sep = "."))
  rng_size <- file.info(rng_file)[["size"]]
  rng <- readBin(rng_file, what = integer(), size = 4L, n = rng_size / 4L, endian = "big")
  dt <- data.table(
    struc = 0L:((length(rng) / 2L) - 1L),
    cpos_left = rng[seq.int(from = 1L, to = length(rng), by = 2L)],
    cpos_right = rng[seq.int(from = 2L, to = length(rng), by = 2L)]
  )
  
  # Now we add the values of the s-attributes to the data.table with regions, one at
  # a time. Again, doing this from the binary files directly is faster than using RcppCWB.
  for (i in seq_along(s_attr)){
    files <- list(
      avs = file.path(x@data_dir, paste(s_attr[i], "avs", sep = ".")),
      avx = file.path(x@data_dir, paste(s_attr[i], "avx", sep = "."))
    )
    sizes <- lapply(files, function(file) file.info(file)[["size"]])
    
    avx <- readBin(files[["avx"]], what = integer(), size = 4L, n = sizes[["avx"]] / 4L, endian = "big")
    avx_matrix <- matrix(avx, ncol = 2, byrow = TRUE)
    
    avs <- readBin(con = files[["avs"]], what = character(), n = sizes[["avs"]])
    if (!is.null(encoding)) Encoding(avs) <- x@encoding
    
    dt[, (s_attr[i]) := avs[match(avx_matrix[, 2], unique(avx_matrix[, 2]))] ]
  }
  
  # Apply the expression.
  if (!missing(subset)){
    setindexv(dt, cols = s_attr)
    dt <- dt[eval(expr, envir = dt)]
  }
  
  if (length(dots) > 0L){
    for (s in s_attr_dots){
      if (regex){
        for (i in length(dots[[s]])){
          dt <- dt[grep(dots[[s]][i], dt[[s]])]
        }
      } else {
        if (length(dots[[s]]) == 1L){
          dt <- dt[dt[[s]] == dots[[s]]]
        } else {
          dt <- dt[dt[[s]] %in% dots[[s]]]
        }
      }
    }
  }
  
  
  # And assemble the subcorpus object that is returned.
  if (nrow(dt) == 0L){
    warning("No matching regions found for the s-attributes provided: Returning NULL object")
    return(NULL)
  }
  
  
  y <- new(
    if (length(x@type) > 0L) paste(x@type, "subcorpus", sep = "_") else "subcorpus",
    corpus = x@corpus,
    encoding = x@encoding,
    type = x@type,
    data_dir = x@data_dir,
    cpos = as.matrix(dt[, c("cpos_left", "cpos_right")]),
    strucs = dt[["struc"]],
    s_attribute_strucs = s_attr[length(s_attr)],
    s_attributes = lapply(setNames(s_attr, s_attr), function(s) unique(dt[[s]])),
    xml = "flat"
  )
  dimnames(y@cpos) <- NULL
  y@size <- size(y)
  y
})


#' @examples 
#' use("polmineR")
#' sc <- subset("GERMAPARLMINI", grepl("Merkel", speaker))
#' sc <- subset("GERMAPARLMINI", speaker == "Angela Dorothea Merkel")
#' sc <- subset("GERMAPARLMINI", speaker == "Angela Dorothea Merkel" & date == "2009-10-28")
#' sc <- subset("GERMAPARLMINI", grepl("Merkel", speaker) & date == "2009-10-28")
#' @rdname subcorpus-class
setMethod("subset", "character", function(x, ...){
  subset(x = corpus(x), ...)
})


#' @examples
#' a <- corpus("GERMAPARLMINI")
#' b <- subset(a, date == "2009-11-10")
#' c <- subset(b, speaker == "Frank-Walter Steinmeier")
#' @rdname subcorpus-class
setMethod("subset", "subcorpus", function(x, subset, ...){
  expr <- substitute(subset)
  
  if (localeToCharset()[1] != x@encoding)
    expr <- .recode_call(x = expr, from = localeToCharset()[1], to = x@encoding)
  
  s_attr <- s_attributes(expr, corpus = x) # get s_attributes present in the expression
  max_attr <- .s_attributes_stop_if_nested(corpus = x@corpus, s_attr = s_attr)

  if (max_attr != cl_attribute_size(corpus = x@corpus, attribute = x@s_attribute_strucs, attribute_type = "s", registry = registry())){
    stop("New s-attributes are nested in existing s-attribute defining subcorpus. ",
         "The method does not (yet) work for nested XML / nested structural attributes.")
  }
  
  dt <- data.table(
    struc = x@strucs,
    cpos_left = x@cpos[,1],
    cpos_right = x@cpos[,2]
  )
  for (s in s_attr){
    str <- RcppCWB::cl_struc2str(corpus = x@corpus, s_attribute = s, struc = dt[["struc"]], registry = registry())
    Encoding(str) <- x@encoding
    dt[, (s) := str]
  }
  setindexv(dt, cols = s_attr)
  dt_min <- dt[eval(expr, envir = dt)]

  y <- new(
    "subcorpus",
    corpus = x@corpus,
    encoding = x@encoding,
    type = x@type,
    data_dir = x@data_dir,
    cpos = as.matrix(dt_min[, c("cpos_left", "cpos_right")]),
    strucs = dt_min[["struc"]],
    s_attribute_strucs = s_attr[length(s_attr)],
    xml = "flat"
  )
  y@size <- size(y)
  y
})


#' @exportMethod show
#' @docType methods
#' @rdname corpus_class
setMethod("show", "corpus", function(object){
  cat(sprintf("** '%s' object **\n", class(object)))
  cat(sprintf("%-12s", "corpus:"), object@corpus, "\n")
  cat(sprintf("%-12s", "encoding:"), object@encoding, "\n")
  cat(sprintf("%-12s", "type:"), if (length(object@type) > 0) object@type else "[undefined]", "\n")
  cat(sprintf("%-12s", "size:"), size(object), "\n")
})




#' @details Applying the `$`-method on a corpus will return the values for the
#'   s-attribute stated with argument \code{name}.
#' @examples
#' g <- corpus("GERMAPARLMINI")
#' g$date
#' corpus("GERMAPARLMINI")$date
#' 
#' sc <- subset("GERMAPARLMINI", date == "2009-10-27")
#' sc$date
#' @exportMethod $
#' @rdname corpus_class
#' @param x An object of class \code{corpus}, or inheriting from it.
#' @param name A (single) s-attribute.
setMethod("$", "corpus", function(x, name){
  s_attributes(x, s_attribute = name)
})

