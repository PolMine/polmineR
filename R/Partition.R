Partition <- R6Class(
  
  classname = "Partition",
  
  # inherit = "Textstat",
  
  public = list(
    
    name = NULL, # character 
    corpus = NULL, # character
    encoding = NULL, # character
    sAttributes = NULL, # list
    explanation = NULL, # character
    cpos = NULL, # matrix
    pos = NULL, # list
    annotations = NULL, # list
    size = NULL, # numeric
    metadata = NULL, # data.frame
    strucs = NULL, # numeric
    stat = NULL, # data.table
    pAttribute = NULL, # character
    xml = NULL, # character
    sAttributeStrucs = NULL, # character
    call = NULL, # character,
    
    initialize = function(...){
      P <- partition(...)
      for (s in names(getSlots("partition"))) self[[s]] <- slot(P, s)
    },
    
    sAttribute = function(){
    },
    
    kwic = function(){
    },
    
    context = function(){
    }
  )
  
  
)
