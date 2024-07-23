setAs(from = "partition", to = "subcorpus", def = function(from){
  y <- new(
    "subcorpus",
    
    # slots inherited from class 'corpus'
    corpus = slot(from, "corpus"),
    registry_dir = slot(from, "registry_dir"),
    data_dir = slot(from, "data_dir"),
    type = if (grepl("^.*?_partition$", class(from))){
        as.vector(gsub("^(.*?)_partition$", "\\1", class(from)))
      } else {
        character()
      }, # slot type does not exist in 'partition' class
    encoding = slot(from, "encoding"),
    
    # slots inherited from class 'regions'
    cpos = slot(from, "cpos"),
    size = slot(from, "size"),
    info_file = slot(from, "info_file"),
    template = slot(from, "template"),
    
    name = unname(slot(from, "name")),

    # slots defined for class 'subcorpus' on its own right
    s_attributes = slot(from, "s_attributes"),
    annotations = list(),
    metadata = slot(from, "metadata"),
    strucs = slot(from, "strucs"),
    xml = slot(from, "xml"),
    s_attribute_strucs = slot(from, "s_attribute_strucs")
  )
  if (length(slot(y, "type")) > 0L){
    if (slot(y, "type") == "plpr") y <- as(y, "plpr_subcorpus")
    if (slot(y, "type") == "press") y <- as(y, "press_subcorpus")
  }
  y
})


setAs(from = "plpr_partition", to = "plpr_subcorpus", def = function(from){
  as(as(from, "subcorpus"), "plpr_subcorpus")
})


setAs(from = "press_partition", to = "press_subcorpus", def = function(from){
  as(as(from, "subcorpus"), "press_subcorpus")
})

