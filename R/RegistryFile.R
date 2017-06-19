#' Read, parse and modify registry file.
#' 
#' @param corpus name of the CWB corpus
#' @param new a new value to set
#' @param filename a filename
#' @param package name of a package
#' @param registry directory of the registry (defaults to CORPUS_Registry environment variable)
#' @field registryDir registry directory
#' @field encoding corpus encoding
#' @field txt registry as character vector 
#' @field pAttributes p-attributes
#' @field properties corpus properties
#' @field id corpus id
#' @field home home directory
#' @field name corpus name
#' @field info path to info file
#' @export RegistryFile
#' @importFrom utils installed.packages
RegistryFile <- setRefClass(
  
  "RegistryFile",
  
  fields = list(
    "package" = "character",
    "registryDir" = "character",
    "filename" = "character",
    "encoding" = "character",
    "txt" = "character",
    "pAttributes" = "character",
    "properties" = "list",
    "id" = "character",
    "home" = "character",
    "name" = "character",
    "info" = "character"
  ),
  
  methods = list(
    
    initialize = function(corpus = NULL, registry = Sys.getenv("CORPUS_REGISTRY"), package = NULL, filename = NULL){
      
      "Initialize a new RegistryFile object."
      
      if (!is.null(filename)){
        if (file.exists(filename)){
          .self$filename <- filename
        } else {
          stop("file does not exist")
        }
      } else {
        if (is.null(package)){
          .self$registryDir <- registry
        } else {
          dir <- system.file("extdata", "cwb", "registry", package = package)
          if (dir.exists(dir)){
            .self$registryDir <- dir
            .self$package <- package
            if (is.null(corpus)) corpus <- list.files(dir, full.names = FALSE)[1]
          } else {
            stop("no CWB registry directory in package")
          }
        }
        .self$filename <- file.path(.self$registryDir, tolower(corpus))
      }
      .self$read()
      
    },
    
    read = function(){
      
      "Read file from disc, as character vector in field 'txt'."
      
      .self$txt <- scan(
        file = file.path(.self$filename),
        sep = "\n",
        what = "character",
        quiet = TRUE, blank.lines.skip = FALSE
      )
      invisible(.self$txt)
    },
    
    getName = function(){
      
      "Get the name of a corpus."
      
      if (length(.self$txt) == 0) .self$read()
      .self$name <- gsub("^NAME\\s+(.*?)\\s*$", "\\1", grep("^NAME.*?$", .self$txt, value = TRUE), perl = TRUE)
      invisible(.self$name)
    },
    
    getId = function(){
      
      "Get the id of a corpus."
      
      if (length(.self$txt) == 0) .self$read()
      .self$id <- gsub("^ID\\s+(.*?)\\s*$", "\\1", grep("^ID.*?$", .self$txt, value = TRUE), perl = TRUE)
      invisible(.self$id)
    },
    
    getHome = function(){
      
      "Get the home directory of a corpus."
      
      if (length(.self$txt) == 0) .self$read()
      .self$home <- gsub("^HOME\\s+(.*?)\\s*$", "\\1", grep("^HOME.*?$", .self$txt, value = TRUE), perl = TRUE)
      .self$home
    },
    
    getInfo = function(){
      
      "Get path to the info file."
      
      if (length(.self$txt) == 0) .self$read()
      .self$info <- gsub("^INFO\\s+(.*?)\\s*$", "\\1", grep("^INFO.*?$", .self$txt, value = TRUE), perl = TRUE)
      invisible(.self$info)
    },
    
    getEncoding = function(){
      
      "Get the encoding."
      
      if (length(.self$txt) == 0) .self$read()
      encodingLine <- .self$txt[grep('charset\\s*=\\s*"', .self$txt)]
      .self$encoding <- sub('^.*charset\\s*=\\s*"(.+?)".*$', "\\1", encodingLine)
      # utf8 is not recognized by iconv on some mac systems
      if (.self$encoding == "utf8") .self$encoding <- "UTF-8"
      if (!toupper(.self$encoding) %in% iconvlist()){
        warning('Please check encoding in the registry file (charset="..." provides unknown encoding) or provide encoding explicitly')
      }
      .self$encoding
    },
    
    getPAttributes = function(){
      
      "Get the pAttributes."
      
      if (length(.self$txt) == 0) .self$read()
      .self$pAttributes <- gsub("^ATTRIBUTE\\s+(.*?)$", "\\1", grep("^ATTRIBUTE", .self$txt, value = TRUE))
      invisible(.self$pAttributes)
    },
    
    getSAttributes = function(){
      
      "Get the sAttributes."
      
      if (length(.self$txt) == 0) .self$read()
      sAttrLines <- grep("\\[annotations\\]", .self$txt)
      gsub("^STRUCTURE\\s+(.*?)\\t.*?$", "\\1", .self$txt[sAttrLines])
      
    },
    
    getProperties = function(){
      
      "Get corpus properties."
      
      if (length(.self$txt) == 0) .self$read()
      propertiesLines <- grep("^##::", .self$txt)
      propertiesNames <- sapply(propertiesLines, function(x) sub("^##::\\s*(.*?)\\s*=.*?$", "\\1", .self$txt[x]))
      .self$properties <- lapply(
        setNames(propertiesLines, propertiesNames),
        function(x) strsplit(gsub('^##::.*?=\\s"(.*?)".*?$', "\\1", .self$txt[x]), "\\|")[[1]]
      )
      invisible(.self$properties)
    },
    
    parse = function(){
      
      "Parse the registry file."
      
      if (length(.self$txt) == 0) .self$read()
      .self$getHome()
      .self$getId()
      .self$getInfo()
      .self$getName()
      .self$getPAttributes()
      .self$getEncoding()
      .self$getProperties()
    },
    
    setHome = function(new){
      if (dir.exists(new)){
        home_position <- grep("^HOME.*?$", .self$txt)
        .self$txt[home_position] <- paste("HOME", new, sep = " ")
        .self$home <- new
      } else {
        stop("directory does not exist")
      }
    },
    
    write = function(filename = NULL){
      if (!is.null(filename)) .self$filename <- filename
      message("... writing registry: ", .self$filename)
      cat(.self$txt, file = .self$filename, sep = "\n")
    },
    
    adjustHome = function(){
      if (.self$package %in% utils::installed.packages()){
        newDir <- system.file("extdata", "cwb", "indexed_corpora", .self$getId(), package = .self$package)
        if (.Platform$OS.type == "windows"){
          newDir <- gsub("^.*?/", "/", newDir)
        }
        .self$setHome(new = newDir)
        .self$write()
      } else {
        stop("adjustHome method to be used only for installed packages")
      }
    }
    
  )
)
