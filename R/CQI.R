#' Interfaces for accessing the CWB
#' 
#' The package offers two different interfaces to the Corpus Workbench (CWB):
#' The package 'RcppCWB', or via cqpserver. An object called 'CQI' will be
#' instantiated in the environment of the polmineR package; the class will
#' provide the functionality to access CWB corpora.
#' @rdname CQI
#' @export CQI.super
#' @importFrom R6 R6Class
#' @importFrom RcppCWB cl_attribute_size cl_lexicon_size cl_cpos2struc cl_cpos2id cl_struc2cpos cl_id2str cl_struc2str
#' @importFrom RcppCWB cl_id2str cl_struc2str cl_regex2id cl_str2id cl_cpos2str cl_id2freq cl_id2cpos cl_cpos2lbound cl_cpos2rbound
#' @importFrom RcppCWB cqp_query cqp_dump_subcorpus
#' @aliases CQI
CQI.super <- R6Class(
  "CQI.super",
  public = list(
    test = function()message("hi there")
  )
)




#' @export CQI.RcppCWB
#' @rdname CQI
#' @importFrom RcppCWB cqp_is_initialized cqp_initialize
CQI.RcppCWB <- R6Class(
  
  "CQI.RcppCWB",
  
  inherit = CQI.super,
  
  public = list(
    
    list_corpora = function(){
      # a very dull solution that does no do any checks!
      toupper(list.files( Sys.getenv("CORPUS_REGISTRY") ))
    },
    
    attributes = function(corpus, type){
      if (type == "p"){
        return( registry_get_p_attributes(corpus) )
      } else if (type == "s"){
        return( registry_get_s_attributes(corpus) )
      }
    },
    
    attribute_size = function(corpus, attribute, type = "s")
      cl_attribute_size(corpus = corpus, attribute = attribute, attribute_type = type, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    lexicon_size = function(corpus, pAttribute)
      cl_lexicon_size(corpus = corpus, p_attribute = pAttribute, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    cpos2struc = function(corpus, sAttribute, cpos)
      cl_cpos2struc(corpus = corpus, s_attribute = sAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    cpos2str = function(corpus, pAttribute, cpos)
      cl_cpos2str(corpus = corpus, p_attribute = pAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    cpos2id = function(corpus, pAttribute, cpos)
      cl_cpos2id(corpus = corpus, p_attribute = pAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    struc2cpos = function(corpus, sAttribute, struc)
      cl_struc2cpos(corpus = corpus, s_attribute = sAttribute, struc = struc, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    id2str = function(corpus, pAttribute, id)
      cl_id2str(corpus = corpus, p_attribute = pAttribute, id = id, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    struc2str = function(corpus, sAttribute, struc)
      cl_struc2str(corpus = corpus, s_attribute = sAttribute, struc = struc, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    regex2id = function(corpus, pAttribute, regex)
      cl_regex2id(corpus = corpus, p_attribute = pAttribute, regex = regex, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    str2id = function(corpus, pAttribute, str)
      cl_str2id(corpus = corpus, p_attribute = pAttribute, str = str, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    id2freq = function(corpus, pAttribute, id)
      cl_id2freq(corpus = corpus, p_attribute = pAttribute, id = id, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    id2cpos = function(corpus, pAttribute, id)
      cl_id2cpos(corpus = corpus, p_attribute = pAttribute, id = id, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    cpos2lbound = function(corpus, sAttribute, cpos)
      cl_cpos2lbound(corpus = corpus, s_attribute = sAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    cpos2rbound = function(corpus, sAttribute, cpos)
      cl_cpos2rbound(corpus = corpus, s_attribute = sAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY")),
    
    query = function(corpus, query){
      if (!RcppCWB::cqp_is_initialized()) cqp_initialize()
      cqp_query(corpus = corpus, query = query)
    },
    
    dump_subcorpus = function(corpus)
      cqp_dump_subcorpus(corpus = corpus)
    
  )
  
)



#' @rdname CQI
#' @export CQI.cqpserver
CQI.cqpserver <- R6Class(
  "CQI.cqpserver",
  inherit = CQI.super,
  
  private = list(
  ),
  
  
  public = list(
    connection = NULL,
    
    authenticate = function(host = "localhost", port = "4877", user = "anonymous", pw = ""){
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
