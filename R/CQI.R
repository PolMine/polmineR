#' Interfaces for accessing the CWB
#' 
#' The package offers three different interfaces to the Corpus
#' Workbench (CWB): The package 'rcqp', via cqpserver, and by
#' calling Perl scripts. An object called 'CQI' will be instantiated
#' in the environment of the polmineR package; the class will 
#' provide the functionality to access CWB corpora.
#' @rdname CQI
#' @export CQI.super
#' @importFrom R6 R6Class
#' @aliases CQI
CQI.super <- R6Class(
  "CQI.super",
  public = list(
    test = function()message("hi there")
  )
)


#' @export CQI.rcqp
#' @rdname CQI
CQI.rcqp <- R6Class(
  
  "CQI.rcqp",
  inherit = CQI.super,
  
  public = list(
    
    list_corpora = function()
      rcqp::cqi_list_corpora(),
    
    attributes = function(corpus, type)
      rcqp::cqi_attributes(corpus, type),
    
    attribute_size = function(corpus, attribute, type = NULL)
      rcqp::cqi_attribute_size(paste(corpus, attribute, sep=".")),
    
    lexicon_size = function(corpus, pAttribute)
      rcqp::cqi_lexicon_size(paste(corpus, pAttribute, sep=".")),
    
    cpos2struc = function(corpus, pAttribute, cpos)
      rcqp::cqi_cpos2struc(paste(corpus, pAttribute, sep="."), cpos),
    
    cpos2str = function(corpus, pAttribute, cpos)
      rcqp::cqi_cpos2str(paste(corpus, pAttribute, sep="."), cpos),
    
    cpos2id = function(corpus, pAttribute, cpos)
      rcqp::cqi_cpos2id(paste(corpus, pAttribute, sep="."), cpos),
    
    struc2cpos = function(corpus, sAttribute, struc)
      rcqp::cqi_struc2cpos(paste(corpus, sAttribute, sep="."), struc),
    
    id2str = function(corpus, pAttribute, id)
      rcqp::cqi_id2str(paste(corpus, pAttribute, sep="."), id),
    
    struc2str = function(corpus, sAttribute, struc)
      rcqp::cqi_struc2str(paste(corpus, sAttribute, sep = "."), struc),
    
    regex2id = function(corpus, pAttribute, regex)
      rcqp::cqi_regex2id(paste(corpus, pAttribute, sep="."), regex),
    
    str2id = function(corpus, pAttribute, str)
      rcqp::cqi_str2id(paste(corpus, pAttribute, sep="."), str),
    
    id2freq = function(corpus, pAttribute, id)
      rcqp::cqi_id2freq(paste(corpus, pAttribute, sep="."), id),
    
    id2cpos = function(corpus, pAttribute, id)
      rcqp::cqi_id2cpos(paste(corpus, pAttribute, sep="."), id),
    
    cpos2lbound = function(corpus, sAttribute, cpos)
      rcqp::cqi_cpos2rbound(paste(corpus, sAttribute, sep="."), cpos),
    
    cpos2rbound = function(corpus, sAttribute, cpos)
      rcqp::cqi_cpos2rbound(paste(corpus, sAttribute, sep="."), cpos),
    
    query = function(corpus, query)
      rcqp::cqi_query(corpus, "Hits", query),
    
    dump_subcorpus = function(corpus){
      rcqp::cqi_dump_subcorpus(paste(corpus, "Hits", sep=":"))
    }
    
  )
)

#' @export CQI.Rcpp
#' @rdname CQI
CQI.Rcpp <- R6Class(
  
  "CQI.Rcpp",
  
  inherit = CQI.super,
  
  public = list(
    
    list_corpora = function(){
      # a very dull solution that does no do any checks!
      toupper(list.files( Sys.getenv("CORPUS_REGISTRY") ))
    },
    
    attributes = function(corpus, type){
      if (type == "p"){
        return( RegistryFile$new(corpus)$getPAttributes() )
      } else if (type == "s"){
        return( RegistryFile$new(corpus)$getSAttributes() )
      }
    },
    
    attribute_size = function(corpus, attribute, type){
      polmineR.Rcpp::attribute_size(corpus = corpus, attribute = attribute, attribute_type = type, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    lexicon_size = function(corpus, pAttribute){
      polmineR.Rcpp::lexicon_size(corpus = corpus, p_attribute = pAttribute, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    cpos2struc = function(corpus, pAttribute, cpos){
      polmineR.Rcpp::cpos2struc(corpus = corpus, p_attribute = pAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    cpos2str = function(corpus, pAttribute, cpos){
      polmineR.Rcpp::cpos2str(corpus = corpus, p_attribute = pAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    cpos2id = function(corpus, pAttribute, cpos){
      polmineR.Rcpp::cpos2id(corpus = corpus, p_attribute = pAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    struc2cpos = function(corpus, sAttribute, struc){
      polmineR.Rcpp::struc2cpos(corpus = corpus, s_attribute = sAttribute, struc = struc, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    id2str = function(corpus, pAttribute, id){
      polmineR.Rcpp::id2str(corpus = corpus, p_attribute = pAttribute, id = id, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    struc2str = function(corpus, sAttribute, struc){
      polmineR.Rcpp::struc2str(corpus = corpus, s_attribute = sAttribute, struc = struc, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    regex2id = function(corpus, pAttribute, regex){
      polmineR.Rcpp::regex2id(corpus = corpus, p_attribute = pAttribute, regex = regex, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    str2id = function(corpus, pAttribute, str){
      polmineR.Rcpp::str2id(corpus = corpus, p_attribute = pAttribute, str = str, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    id2freq = function(corpus, pAttribute, id){
      polmineR.Rcpp::id2freq(corpus = corpus, p_attribute = pAttribute, id = id, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    id2cpos = function(corpus, pAttribute, id){
      polmineR.Rcpp::id2cpos(corpus = corpus, p_attribute = pAttribute, id = id, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    cpos2lbound = function(corpus, sAttribute, cpos){
      polmineR.Rcpp::cpos2lbound(corpus = corpus, s_attribute = sAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    cpos2rbound = function(corpus, sAttribute, cpos){
      polmineR.Rcpp::cpos2rbound(corpus = corpus, s_attribute = sAttribute, cpos = cpos, registry = Sys.getenv("CORPUS_REGISTRY"))
    },
    
    query = function(corpus, query)
      stop("Not possible to implement this with Rcpp/CL!"),
    
    dump_subcorpus = function(corpus){
      stop("Not possible to implement this with Rcpp/CL!")
    }
    
  )
  
)

#' @rdname CQI
#' @export CQI.perl
CQI.perl <- R6Class(
  
  "CQI.perl",
  inherit = CQI.super,
  public = list(
    
    as.cmd = function(cmd) paste("perl -e '", paste(cmd, collapse = " "), "'", sep=""),
    
    list_corpora = function() stop("list_corpora not implemented for perl interface"),
    
    attribute_size = function(corpus, attribute, type = NULL){
      as.integer(system(sprintf(self$as.cmd(.cqi_perl[["attribute_size"]]), corpus, attribute), intern=TRUE))
    },
    
    lexicon_size = function(corpus, pAttribute){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["lexicon_size"]]), corpus, pAttribute),
          intern=TRUE
        )
      )
    },
    
    cpos2struc = function(corpus, pAttribute, cpos){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["cpos2struc"]]), corpus, pAttribute, paste(as.character(cpos), collapse=",")),
          intern=TRUE)
      )
    },
    
    cpos2str = function(corpus, pAttribute, cpos){
      system(
        sprintf(self$as.cmd(.cqi_perl[["cpos2str"]]), corpus, pAttribute, paste(as.character(cpos), collapse=",")),
        intern=TRUE
      )
    },
    
    cpos2id = function(corpus, pAttribute, cpos){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["cpos2id"]]), corpus, pAttribute, paste(as.character(cpos), collapse=",")),
          intern=TRUE)
      )
    },
    
    struc2cpos = function(corpus, sAttribute, struc){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["struc2cpos"]]), corpus, sAttribute, struc),
          intern=TRUE)
      )
    },
    
    id2str = function(corpus, pAttribute, id){
      system(
        sprintf(self$as.cmd(.cqi_perl[["id2str"]]), corpus, pAttribute, paste(as.character(id), collapse=",")),
        intern=TRUE
      )
    },
    
    struc2str = function(corpus, sAttribute, struc){
      system(
        sprintf(
          self$as.cmd(.cqi_perl[["struc2str"]]), corpus, sAttribute, paste(as.character(struc), collapse=",")),
        intern=TRUE
      )
    },
    
    regex2id= function(corpus, pAttribute, regex){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["regex2id"]]), corpus, pAttribute, regex),
          intern=TRUE
        )
      )
    },
    
    str2id = function(corpus, pAttribute, str){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["str2id"]]), corpus, pAttribute, paste('"', str, '"', sep="", collapse=",")),
          intern=TRUE
        )
      )
    },
    
    id2freq = function(corpus, pAttribute, id){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["id2freq"]]), corpus, pAttribute, paste(id, collapse=",")),
          intern=TRUE
        )
      )
    },
    
    id2cpos = function(corpus, pAttribute, id){
      as.integer(
        system(
          sprintf(self$as.cmd(.cqi_perl[["id2cpos"]]), corpus, pAttribute, paste(id, collapse=",")),
          intern=TRUE
        )
      )
    },
    
    cpos2lbound = function(corpus, sAttribute, cpos){
      message("cpos2lbound not implemented in the Perl-CL-Package")
    },
    
    cpos2rbound = function(corpus, sAttribute, cpos){
      message("cpos2rbound not implemented in the Perl-CL-Package")
    },
    
    query = function(){
      message("query not yet implemented")
    },
    
    dump_subcorpus = function(){
      message("dump_subcorpus not yet implemented")
    }
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

CQI <- switch(
  Sys.getenv("POLMINER_INTERFACE"),
  "rcqp" = CQI.rcqp$new(),
  "perl" = CQI.cqpserver$new(),
  "cqpserver" = CQI.cqpserver$new(),
  if (requireNamespace("rcqp", quietly = TRUE)) CQI.rcqp$new() else CQI.perl$new()
)
