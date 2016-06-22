#' @importFrom rcqp cqi_cpos2id cqi_cpos2str
#' @importFrom rcqp cqi_cpos2id
#' @importFrom rcqp cqi_regex2id
#' @importFrom rcqp cqi_id2str cqi_str2id cqi_id2freq cqi_id2cpos
#' @importFrom rcqp cqi_lexicon_size cqi_list_corpora cqi_attribute_size cqi_attributes
#' @importFrom rcqp cqi_struc2str cqi_struc2cpos
#' @importFrom rcqp cqi_query cqi_dump_subcorpus
#' @importFrom rcqp cqi_cpos2lbound cqi_cpos2rbound
#' @importFrom rcqp cqi_struc2str cqi_cpos2str cqi_cpos2struc
#' @importFrom R6 R6Class
#' @export CQI.rcqp
#' @rdname CQI
CQI.rcqp <- R6Class(
  
  "CQI.rcqp",
  inherit = CQI.super,
  
  public = list(
    
    list_corpora = function()
      cqi_list_corpora(),
    
    attributes = function(corpus, type)
      cqi_attributes(corpus, type),
    
    attribute_size = function(corpus, attribute)
      cqi_attribute_size(paste(corpus, attribute, sep=".")),
    
    lexicon_size = function(corpus, pAttribute)
      cqi_lexicon_size(paste(corpus, pAttribute, sep=".")),
    
    cpos2struc = function(corpus, pAttribute, cpos)
      cqi_cpos2struc(paste(corpus, pAttribute, sep="."), cpos),
    
    cpos2str = function(corpus, pAttribute, cpos)
      cqi_cpos2str(paste(corpus, pAttribute, sep="."), cpos),
    
    cpos2id = function(corpus, pAttribute, cpos)
      cqi_cpos2id(paste(corpus, pAttribute, sep="."), cpos),
    
    struc2cpos = function(corpus, sAttribute, struc)
      cqi_struc2cpos(paste(corpus, sAttribute, sep="."), struc),
    
    id2str = function(corpus, pAttribute, id)
      cqi_id2str(paste(corpus, pAttribute, sep="."), id),
    
    struc2str = function(corpus, sAttribute, struc)
      cqi_struc2str(paste(corpus, sAttribute, sep="."), struc),
    
    regex2id = function(corpus, pAttribute, regex)
      cqi_regex2id(paste(corpus, pAttribute, sep="."), regex),
    
    str2id = function(corpus, pAttribute, str)
      cqi_str2id(paste(corpus, pAttribute, sep="."), str),
    
    id2freq = function(corpus, pAttribute, id)
      cqi_id2freq(paste(corpus, pAttribute, sep="."), id),
    
    id2cpos = function(corpus, pAttribute, id)
      cqi_id2cpos(paste(corpus, pAttribute, sep="."), id),
    
    cpos2lbound = function(corpus, sAttribute, cpos)
      cqi_cpos2rbound(paste(corpus, sAttribute, sep="."), cpos),
    
    cpos2rbound = function(corpus, sAttribute, cpos)
      cqi_cpos2rbound(paste(corpus, sAttribute, sep="."), cpos),
    
    query = function(corpus, query)
      cqi_query(corpus, "Hits", query),
    
    dump_subcorpus = function(corpus){
      cqi_dump_subcorpus(paste(corpus, "Hits", sep=":"))
    }
    
  )
  
)