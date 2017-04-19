#' start CQP server 
#' 
#' The function will start the CQP server by way of a system call 
#' to cqpserver. 
#' 
#' @param registryDir path to the registry directory
#' @param initFile path to the init file required by cqpserver
#' @param debugMode logical, whether to run debug mode
#' @param exec logical, whether to start the server right away, or return
#' a command that can be run in the shall
#' @export startServer
#' @rdname cqpserver
#' @name cqpserver
startServer <- function(
  registryDir=Sys.getenv("CORPUS_REGISTRY"),
  initFile=system.file("init", "cqpserver.init", package="cqi"),
  debugMode=TRUE,
  exec=TRUE
){
  cmdRaw <- c(
    "cqpserver",
    "-I", initFile,
    "-r", registryDir,
    ifelse(debugMode, "-d ALL", "")
  )
  cmd <- paste(cmdRaw, collapse=" ")
  if (exec == TRUE) {
    system(cmd)
  } else {
    return(cmd)
  }
}

####################
#                  #
# helper functions #
#                  #
####################

.rawToMsg <- function(x){
  .cqiMsg[as.character(as.integer(x[1])*256 + as.integer(x[2]))]
}

.longIntToRaw <- function(x){
  eightDigits <- as.character(format(as.hexmode(x), 8))
  chunks <- sapply(c(0:3), function(i) substr(eightDigits, start=i*2+1, stop=i*2+2))
  unname(sapply(chunks, function(i) as.raw(as.hexmode(i))))
}

.rawToInt <- function(x){
  result <- as.integer(x)
  as.integer(sum(result * (256^((length(result):1) - 1))))
}


.hexToRaw <- function(x){
  xHexs <- format(as.hexmode(unlist(x)), width=4)
  xAsRaw <- lapply(
    xHexs,
    function(xHex){
      c(
        as.raw(as.hexmode(substr(xHex, start=1, stop=2))),
        as.raw(as.hexmode(substr(xHex, start=3, stop=4)))
      )
    })
  names(xAsRaw) <- names(x)
  xAsRaw
}

.cqiCmdSpec <- list(
  "CQI_STATUS_OK" = 0x0101,
  "CQI_STATUS_CONNECT_OK" = 0x0102,
  "CQI_STATUS_BYE_OK" = 0x0103,
  "CQI_STATUS_PING_OK" = 0x0104,
  "CQI_ERROR_GENERAL_ERROR" = 0x0201,
  "CQI_ERROR_CONNECT_REFUSED" = 0x0202,
  "CQI_ERROR_USER_ABORT" = 0x0203,
  "CQI_ERROR_SYNTAX_ERROR" = 0x0204,
  "CQI_DATA_BYTE" = 0x0301,
  "CQI_DATA_BOOL" = 0x0302,
  "CQI_DATA_INT" = 0x0303,
  "CQI_DATA_STRING" = 0x0304,
  "CQI_DATA_BYTE_LIST" = 0x0305,
  "CQI_DATA_BOOL_LIST" = 0x0306,
  "CQI_DATA_INT_LIST" = 0x0307,
  "CQI_DATA_STRING_LIST" = 0x0308,
  "CQI_DATA_INT_INT" = 0x0309,
  "CQI_DATA_INT_INT_INT_INT" = 0x030A,
  "CQI_DATA_INT_TABLE" = 0x030B,
  "CQI_CL_ERROR_NO_SUCH_ATTRIBUTE" = 0x0401,
  "CQI_CL_ERROR_WRONG_ATTRIBUTE_TYPE" = 0x0402,
  "CQI_CL_ERROR_OUT_OF_RANGE" = 0x0403,
  "CQI_CL_ERROR_REGEX" = 0x0404,
  "CQI_CL_ERROR_CORPUS_ACCESS" = 0x0405,
  "CQI_CL_ERROR_OUT_OF_MEMORY" = 0x0406,
  "CQI_CL_ERROR_INTERNAL" = 0x0407,
  "CQI_CQP_ERROR_GENERAL" = 0x0501,
  "CQI_CQP_ERROR_NO_SUCH_CORPUS" = 0x0502,
  "CQI_CQP_ERROR_INVALID_FIELD" = 0x0503,
  "CQI_CQP_ERROR_OUT_OF_RANGE" = 0x0504,
  "CQI_CTRL_CONNECT" = 0x1101,
  "CQI_CTRL_BYE" = 0x1102,
  "CQI_CTRL_USER_ABORT" = 0x1103,
  "CQI_CTRL_PING" = 0x1104,
  "CQI_CTRL_LAST_GENERAL_ERROR" = 0x1105,
  "CQI_ASK_FEATURE_CQI_1_0" = 0x1201,
  "CQI_ASK_FEATURE_CL_2_3" = 0x1202,
  "CQI_ASK_FEATURE_CQP_2_3" = 0x1203,
  "CQI_CORPUS_LIST_CORPORA" = 0x1301,
  "CQI_CORPUS_CHARSET" = 0x1303,
  "CQI_CORPUS_PROPERTIES" = 0x1304,
  "CQI_CORPUS_POSITIONAL_ATTRIBUTES" = 0x1305,
  "CQI_CORPUS_STRUCTURAL_ATTRIBUTES" = 0x1306,
  "CQI_CORPUS_STRUCTURAL_ATTRIBUTE_HAS_VALUES" = 0x1307,
  "CQI_CORPUS_ALIGNMENT_ATTRIBUTES" = 0x1308,
  "CQI_CORPUS_FULL_NAME" = 0x1309,
  "CQI_CORPUS_INFO" = 0x130A,
  "CQI_CORPUS_DROP_CORPUS" = 0x130B,
  "CQI_CL_ATTRIBUTE_SIZE" = 0x1401,
  "CQI_CL_LEXICON_SIZE" = 0x1402,
  "CQI_CL_DROP_ATTRIBUTE" = 0x1403,
  "CQI_CL_STR2ID" = 0x1404,
  "CQI_CL_ID2STR" = 0x1405,
  "CQI_CL_ID2FREQ" = 0x1406,
  "CQI_CL_CPOS2ID" = 0x1407,
  "CQI_CL_CPOS2STR" = 0x1408,
  "CQI_CL_CPOS2STRUC" = 0x1409,
  "CQI_CL_CPOS2LBOUND" = 0x1420,
  "CQI_CL_CPOS2RBOUND" = 0x1421,
  "CQI_CL_CPOS2ALG" = 0x140A,
  "CQI_CL_STRUC2STR" = 0x140B,
  "CQI_CL_ID2CPOS" = 0x140C,
  "CQI_CL_IDLIST2CPOS" = 0x140D,
  "CQI_CL_REGEX2ID" = 0x140E,
  "CQI_CL_STRUC2CPOS" = 0x140F,
  "CQI_CL_ALG2CPOS" = 0x1410,
  "CQI_CQP_QUERY" = 0x1501,
  "CQI_CQP_LIST_SUBCORPORA" = 0x1502,
  "CQI_CQP_SUBCORPUS_SIZE" = 0x1503,
  "CQI_CQP_SUBCORPUS_HAS_FIELD" = 0x1504,
  "CQI_CQP_DUMP_SUBCORPUS" = 0x1505,
  "CQI_CQP_DROP_SUBCORPUS" = 0x1509,
  "CQI_CQP_FDIST_1" = 0x1510,
  "CQI_CQP_FDIST_2" = 0x1511
)


.cqiCmd <- .hexToRaw(.cqiCmdSpec)
.cqiMsg <- setNames(names(.cqiCmdSpec), unlist(.cqiCmdSpec))


#' @rdname CQI
#' @export CQI.cqpserver
CQI.cqpserver <- R6Class(
  "CQI.cqpserver",
  inherit = CQI.super,
  
  private = list(
  ),
  
  
  public = list(
    connection = NULL,
    
    authenticate = function(host="localhost", port="4877", user="anonymous", pw=""){
      self$connection <- socketConnection(host, port, open="wb")
      self$send_word(.cqiCmd[["CQI_CTRL_CONNECT"]])
      self$send_string(user)
      self$send_string(pw)
      Sys.sleep(0.1)
      status <- self$read_word()
      message("... ", .rawToMsg(status))
    },
    
    send_byte = function(x){},
    send_byte_list = function(x){},
    read_byte = function(){},
    read_byte_list = function(){},
    
    
    send_int = function(x){
      writeBin(.longIntToRaw(x), self$connection)  
    },
    
    send_int_list = function(x){
      writeBin(.longIntToRaw(length(x)), self$connection)  
      invisible(sapply(x, self$send_int))
    },
    
    send_word = function(x){
      writeBin(c(as.raw(0), unname(unlist(x))), self$connection)
    },
    
    send_word_list = function(x){
    },
    
    
    read_word = function(n=2){
      readBin(self$connection, what="raw", n=n)
    },
    
    read_word_list = function(){},
    
    read_string = function(){
      strLengthRaw <- self$read_word()
      strLength <- as.integer(strLengthRaw[length(strLengthRaw)])
      strRaw <- readBin(self$connection, what="raw", n=strLength)
      rawToChar(strRaw)
    },
    
    expect_string = function(){
      self$read_word()
      self$read_string()
    },
    
    send_string = function(x){
      writeBin(.longIntToRaw(nchar(x))[3:4], self$connection)
      writeLines(x, self$connection, sep="")
    },
    
    send_string_list = function(strs){
      self$send_int(length(strs))
      invisible(sapply(strs, self$send_string))
    },
    
    read_int = function(){
      lenRaw <- readBin(self$connection, what="raw", n=4)
      .rawToInt(lenRaw)
    },
    
    read_int_list = function(){
      len <- self$read_int()
      sapply(c(1:len), function(x)self$read_int())
    },
    
    expect_int = function(){
      #status <- readBin(self$connection, what="raw", n=2)
      self$read_word()
      self$read_int()
    },
    
    expect_int_list = function(){
      self$read_word() # status message
      len <- self$read_int()
      sapply(c(1:len), function(i) self$read_int())
    },
    
    read_int_table = function(){},
    
    read_string_list = function(){
      len <- self$read_int()
      sapply(c(1:len), function(i) self$read_string())
    },
    
    expect_string_list = function(){
      self$read_word()
      self$read_string_list()
    },
    
    ##### public ####
    
    list_corpora = function(){
      self$send_word(.cqiCmd[["CQI_CORPUS_LIST_CORPORA"]])
      Sys.sleep(0.1)
      self$read_word()
      self$read_string_list()
    },
    
    attributes = function(corpus, type){
      if (type == "p"){
        self$send_word(.cqiCmd[["CQI_CORPUS_POSITIONAL_ATTRIBUTES"]])
        self$send_string(corpus)
      } else if (type == "s"){
        self$send_word(.cqiCmd[["CQI_CORPUS_STRUCTURAL_ATTRIBUTES"]])
        self$send_string(corpus)
      }
      # Sys.sleep(0.1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      self$expect_string_list()
    },
    
    attribute_size = function(corpus, attribute, type = NULL){
      self$send_word(.cqiCmd[["CQI_CL_ATTRIBUTE_SIZE"]])
      self$send_string(paste(corpus, attribute, sep="."))
      # Sys.sleep(0.1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      self$expect_int()
    },
    
    lexicon_size = function(corpus, pAttribute){
      self$send_word(.cqiCmd[["CQI_CL_LEXICON_SIZE"]])
      self$send_string(paste(corpus, pAttribute, sep="."))
      # Sys.sleep(0.1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      self$expect_int()
    },
    
    charset = function(corpus){
      self$send_word(.cqiCmd[["CQI_CORPUS_CHARSET"]])
      self$send_string(corpus)
      # Sys.sleep(0.1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      self$expect_string()
    },
    
    cpos2struc = function(corpus, pAttribute, cpos){
      self$send_word(.cqiCmd[["CQI_CL_CPOS2STRUC"]])
      self$send_string(paste(corpus, pAttribute, sep="."))
      self$send_int_list(cpos)
      # Sys.sleep(0.1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      self$expect_int_list()
    },
    
    cpos2str = function(corpus, pAttribute, cpos){
      self$send_word(.cqiCmd[["CQI_CL_CPOS2STR"]])
      self$send_string(paste(corpus, pAttribute, sep="."))
      self$send_int_list(cpos)
      # Sys.sleep(0.1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      self$expect_string_list()
    },
    
    cpos2id = function(corpus, pAttribute, cpos){
      self$send_word(.cqiCmd[["CQI_CL_CPOS2ID"]])
      self$send_string(paste(corpus, pAttribute, sep="."))
      self$send_int_list(cpos)
      # Sys.sleep(0.1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      self$expect_int_list()
    },
    
    struc2cpos = function(corpus, sAttribute, struc){
      self$send_word(.cqiCmd[["CQI_CL_STRUC2CPOS"]])
      self$send_string(paste(corpus, sAttribute, sep="."))
      self$send_int(struc)
      # Sys.sleep(0.1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      cposLeft <- self$expect_int()
      cposRight <- self$read_int()
      c(cposLeft, cposRight)
    },
    
    id2str = function(corpus, pAttribute, ids){
      self$send_word(.cqiCmd[["CQI_CL_ID2STR"]])
      self$send_string(paste(corpus, pAttribute, sep="."))
      self$send_int_list(ids)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      # Sys.sleep(0.1)
      self$expect_string_list()
    },
    
    struc2str = function(corpus, sAttribute, struc){
      self$send_word(.cqiCmd[["CQI_CL_STRUC2STR"]])
      self$send_string(paste(corpus, sAttribute, sep="."))
      self$send_int_list(struc)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      # Sys.sleep(0.1)
      self$expect_string_list()
    },
    
    regex2id = function(corpus, pAttribute, regex){
      
    },
    
    str2id = function(corpus, pAttribute, strs){
      self$send_word(.cqiCmd[["CQI_CL_STR2ID"]])
      self$send_string(paste(corpus, pAttribute, sep="."))
      self$send_string_list(strs)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      # Sys.sleep(0.1)
      self$expect_int_list()
    },
    
    id2freq = function(corpus, pAttribute, id){
    },
    
    id2cpos = function(corpus, pAttribute, id){
      self$send_word(.cqiCmd[["CQI_CL_ID2CPOS"]])
      self$send_string(paste(corpus, pAttribute, sep="."))
      self$send_int(id)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.1)
      self$read_word()
      self$read_int_list()
    },
    
    cpos2lbound = function(corpus, sAttribute, cpos){
    },
    
    cpos2rbound = function(corpus, sAttribute, cpos){
    },
    
    query = function(corpus, query){
      self$send_word(.cqiCmd[["CQI_CQP_QUERY"]])
      self$send_string(corpus)
      self$send_string("Hits")
      self$send_string(query)
      # Sys.sleep(1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      status <- self$read_word()
      message("... ", .rawToMsg(status))
    },
    
    subcorpus_size = function(corpus) {
      self$send_word(.cqiCmd[["CQI_CQP_SUBCORPUS_SIZE"]])
      self$send_string(paste(corpus, "Hits", sep=":"))
      # Sys.sleep(0.1)
      while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
      self$expect_int()
    },
    
    dump_subcorpus = function(corpus){
      last <- self$subcorpus_size(corpus)
      beginAndEnd <- lapply(
        c(0x10, 0x11),
        function(what){
          self$send_word(.cqiCmd[["CQI_CQP_DUMP_SUBCORPUS"]])
          self$send_string(paste(corpus, "Hits", sep=":"))
          writeBin(.hexToRaw(what)[[1]][2], self$connection)
          self$send_int(0)
          self$send_int(last - 1)
          # Sys.sleep(0.1)
          while (socketSelect(list(self$connection)) == FALSE) Sys.sleep(0.01)
          self$expect_int_list()
        }
      )
      do.call(cbind, beginAndEnd)
    }
    
  )
)
