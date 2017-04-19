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
      rcqp::cqi_struc2str(paste(corpus, sAttribute, sep="."), struc),
    
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