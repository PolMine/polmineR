setAs(from = "partition", to = "subcorpus", def = function(from){
  y <- new(
    "subcorpus",
    
    # slots inherited from class 'corpus'
    corpus = from@corpus,
    registry_dir = from@registry_dir,
    data_dir = from@data_dir,
    type = if (grepl("^.*?_partition$", class(from))){
        as.vector(gsub("^(.*?)_partition$", "\\1", class(from)))
      } else {
        character()
      }, # slot type does not exist in 'partition' class
    encoding = from@encoding,
    
    # slots inherited from class 'regions'
    cpos = from@cpos,
    size = from@size,
    info_file = from@info_file,
    template = from@template,
    
    name = unname(from@name),

    # slots defined for class 'subcorpus' on its own right
    s_attributes = from@s_attributes,
    annotations = list(),
    metadata = from@metadata,
    strucs = from@strucs,
    xml = from@xml,
    s_attribute_strucs = from@s_attribute_strucs
  )
  if (length(y@type) > 0L){
    if (y@type == "plpr") y <- as(y, "plpr_subcorpus")
    if (y@type == "press") y <- as(y, "press_subcorpus")
  }
  y
})


setAs(from = "plpr_partition", to = "plpr_subcorpus", def = function(from){
  as(as(from, "subcorpus"), "plpr_subcorpus")
})


setAs(from = "press_partition", to = "press_subcorpus", def = function(from){
  as(as(from, "subcorpus"), "press_subcorpus")
})

