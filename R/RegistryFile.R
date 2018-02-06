#' Read, parse and modify registry file.
#' 
#' The class includes methods to read, modify and write a registry file.
#' Several operations could be accomplished with the 'cwb-regedit' tool,
#' the functions defined here ensure that manipulating the registry is 
#' possible without a full installation of the CWB.
#' 
#' An appendix to the 'Corpus Encoding Tutorial' (http://cwb.sourceforge.net/files/CWB_Encoding_Tutorial.pdf)
#' includes an explanation of the registry file format. 
#' 
#' @param corpus name of the CWB corpus
#' @param new a new value to set
#' @param filename a filename
#' @param package name of a package
#' @param registry directory of the registry (defaults to CORPUS_Registry environment variable)
#' @param verbose logical, whether to output information
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
      invisible(.self)
    },

    read = function(){

      "Read file from disc, as character vector in field 'txt'."

      .self$txt <- readLines(.self$filename)
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

    setId = function(new){

      "Set the id of a corpus"

      if (length(.self$txt) == 0) .self$read()
      idline <- grep("^ID\\s+.*?$", .self$txt)
      .self$txt[idline] <- sprintf("ID %s", new)
      .self$id <- new

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
      if (grepl('".*?"', .self$info)) .self$info <- gsub('"(.*?)"', "\\1", .self$info)
      .self$info
    },

    setInfo = function(new){
      
      "Reset path to info file."
      
      infoLine <- grep("^INFO\\s+.*?$", .self$txt)
      .self$txt[infoLine] <- sprintf("INFO %s", new)
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

    addPAttribute = function(pAttribute){

      "Add an p-attribute."

      if (pAttribute %in% .self$getPAttributes()){
        stop("doing nothing - sAttribute already declared")
      }
      pAttrLines <- grep("^ATTRIBUTE", .self$txt)
      .self$txt <- c(
        .self$txt[1:pAttrLines[length(pAttrLines)]],
        sprintf("ATTRIBUTE %s", pAttribute),
        .self$txt[(pAttrLines[length(pAttrLines)] + 2):length(.self$txt)]
      )
      .self$write()
      .self$getPAttributes()
    },


    getSAttributes = function(){

      "Get the sAttributes."

      if (length(.self$txt) == 0) .self$read()
      sAttrLines <- grep("^STRUCTURE.*?$", .self$txt)
      y <- gsub("^STRUCTURE\\s+(.*?)(|\\s+.*?)$", "\\1", .self$txt[sAttrLines], perl = TRUE)
      return(y)
    },

    addSAttribute = function(sAttribute){

      "Add an s-attribute."

      if (sAttribute %in% .self$getSAttributes()){
        stop("doing nothing - sAttribute already declared")
      }
      if (getOption("polmineR.cwb-regedit") == FALSE){
        sAttrLines <- grep("^STRUCTURE", .self$txt)
        if (length(sAttrLines) == 0){
          if (grepl("^##\\ss-attributes", .self$txt)){
            sAttrLine <- grep("^##\\ss-attributes", .self$txt) + 1
            .self$txt <- c(
              .self$txt[1:(sAttrLine + 1)],
              "",
              sprintf("STRUCTURE %s # [annotations]", sAttribute),
              .self$txt[(sAttrLine + 2):length(.self$txt)]
            )
          } else {
            .self$txt <- c(
              .self$txt, "##", "## s-attributes (structural markup)", "##",
              sprintf("STRUCTURE %s # [annotations]", sAttribute)
              )
          }
        } else {
          sAttrLines <- grep("^STRUCTURE", .self$txt)
          .self$txt <- c(
            .self$txt[1:sAttrLines[length(sAttrLines)]],
            sprintf("STRUCTURE %s # [annotations]", sAttribute),
            .self$txt[(sAttrLines[length(sAttrLines)] + 2):length(.self$txt)]
          )
        }
      } else {
        cmd <- c(
          "cwb-regedit",
          sprintf("--registry=%s", Sys.getenv("CORPUS_REGISTRY")),
          tolower(.self$getId()),
          ":add", ":s", sAttribute
        )
        system(paste(cmd, collapse = " "))
        .self$read()
      }
      .self$getSAttributes()
    },

    dropSAttribute = function(sAttribute){

      "Drop a s-attribute."

      if (!sAttribute %in% .self$getSAttributes()){
        stop("doing nothing - sAttribute not yet declared")
      }
      sAttrLine <- grep(sprintf("$STRUCTURE %s", sAttribute), .self$txt)
      .self$txt <- c(
        .self$txt[1:(sAttrLine - 1)],
        .self$txt[(sAttrLine + 1):length(.self$txt)]
      )
      .self$getSAttributes()
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

    setProperty = function(property, value){

      "Set a corpus property."

      props <- .self$getProperties()
      new_line <- sprintf('##:: %s = "%s"', property, value)
      if (property %in% names(props)){
        line_no <- grep(sprintf("^##::\\s*%s", property), .self$txt)
        .self$txt[line_no] <- new_line
      } else {
        property_lines <- grep("^##::", .self$txt)
        last_property_line <- property_lines[length(property_lines)]
        .self$txt <- c(
          .self$txt[1:last_property_line],
          new_line,
          .self$txt[(last_property_line + 1):length(.self$txt)]
        )
      }
      .self$getProperties()
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

      "Set the home directory to a new location."

      if (dir.exists(new)){
        home_position <- grep("^HOME.*?$", .self$txt)
        .self$txt[home_position] <- paste("HOME", new, sep = " ")
        .self$home <- new
      } else {
        stop("directory does not exist")
      }
    },

    write = function(filename = NULL, verbose = TRUE){

      "Write registry file to disk."

      if (!is.null(filename)) .self$filename <- filename
      .message("writing registry: ", .self$filename, verbose = verbose)
      cat(.self$txt, file = .self$filename, sep = "\n")
    },

    adjustHome = function(){

      "Reset the home directory. This will usually be necessary after installing a data package."

      if (.self$package %in% utils::installed.packages()){
        newDir <- system.file("extdata", "cwb", "indexed_corpora", .self$getId(), package = .self$package)
        if (.Platform$OS.type == "windows"){
          newDir <- gsub("^[A-Z]?:?(.*)$", "\\1", newDir)
        }
        .self$setHome(new = newDir)
        .self$write()
      } else {
        stop("adjustHome method to be used only for installed packages")
      }
    }

  )
)
