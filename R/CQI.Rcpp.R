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