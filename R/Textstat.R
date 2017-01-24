Textstat <- R6Class(
  classname = "Textstat",
  
  public = list(
    
    corpus = NULL, # character
    pAttribute = NULL, # character
    encoding = NULL, # character
    stat = NULL, # data.table
    name = NULL # character
  
  )
)
