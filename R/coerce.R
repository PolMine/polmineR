setAs(from = "partition", to = "subcorpus", def = function(from){
  new(
    "subcorpus",
    
    # slots inherited from class 'corpus'
    corpus = from@corpus,
    data_dir = character(),
    type = character(), # slot type does not exist in 'partition' class
    encoding = from@encoding,
    
    # slots inherited from class 'regions'
    cpos = from@cpos,
    size = from@size,
    
    # slots defined for class 'subcorpus' on its own right
    s_attributes = from@s_attributes,
    annotations = list(),
    metadata = from@metadata,
    strucs = from@strucs,
    xml = from@xml,
    s_attribute_strucs = from@s_attribute_strucs
  )
})