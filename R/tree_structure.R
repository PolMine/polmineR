#' Show the structure of s-attributes
#' 
#' Show the structure of s-attributes. If `x` is a `subcorpus`, the s-attribute
#' used for corpus subsetting is highlighted.
#' @exportMethod tree_structure
#' @rdname tree_structure
#' @param x Object for which to visualise structure.
#' @param s_attribute Name of the s-attribute used for subsetting, will be
#'   highlighted.
#' @param prefix Number of blank spaces.
#' @param indent Number of spaces to indent.
#' @param root Whether branch is root.
#' @param ... Further arguments.
setGeneric("tree_structure", function(x, ...) standardGeneric("tree_structure"))

#' @importFrom cli bg_br_yellow col_red
#' @importFrom xml2 xml_child xml_contents xml_type
#' @rdname tree_structure
setMethod("tree_structure", "xml_node", function(x, s_attribute = NULL, prefix = 0, indent = 3, root = TRUE){
  padding <- paste(rep(" ", prefix), collapse = "")
  type <- xml_type(x)
  
  if (type == "element") {
    
    if (!is.null(s_attribute)){
      s_attribute_splitted <- strsplit(s_attribute, "_")[[1]]
      if (length(s_attribute_splitted) > 1L){
        active_attr <- paste(
          s_attribute_splitted[2L:length(s_attribute_splitted)],
          collapse = "_", sep = ""
        ) 
      } else{
        active_attr <- ""
      }
    } else {
      active_attr <- ""
    }

    attr_col <- unlist(lapply(
      names(xml_attrs(x)),
      function(att){
        if (att == active_attr && s_attribute_splitted[[1]] == xml_name(x))
          col_red(att)
        else
          att
      }
    ))
    
    if (length(attr_col) > 0){
      attr_str <- paste0(" [", paste0(attr_col, collapse = "\u2502"), "]")
    } else {
      attr_str <- ""
    }

    node <- paste0(xml_name(x), attr_str)
    if (!is.null(s_attribute))
      if (xml_name(x) == strsplit(s_attribute, "_")[[1]][[1]])
        node <- bg_br_yellow(node)
    
    if (isFALSE(root)){
      cat(paste(rep(" ", prefix - 1L), collapse = ""), "|", "\n")
      branching <- "\u2514\u2500 "
    } else {
      branching <- ""
    }
    
    cat(padding, branching,  node, "\n", sep = "")
    
    lapply(
      xml_contents(x),
      tree_structure,
      prefix = prefix + indent,
      indent = indent,
      s_attribute = s_attribute,
      root = FALSE
    )
  }
  invisible(NULL)
})


#' @rdname tree_structure
setMethod("tree_structure", "xml_document", function(x, s_attribute = NULL, prefix = 0, indent = 3, root = TRUE){
  tree_structure(
    x = xml_child(xml_find_first(x, xpath = "/")),
    s_attribute = s_attribute,
    prefix = prefix,
    indent = indent,
    root = root
  )
})

#' @examples
#' xml_sample <- system.file(
#'   package = "GermaParl2",
#'   "extdata", "cwb", "indexed_corpora",
#'   "germaparl2mini"
#' )
#' if (nchar(xml_sample) > 0L && interactive()){
#'   use(pkg = "GermaParl2", corpus = "GERMAPARL2MINI")
#'   
#'   corpus("GERMAPARL2MINI") %>%
#'     tree_structure()
#'     
#'   corpus("GERMAPARL2MINI") %>%
#'     subset(speaker_name == "Konrad Adenauer") %>%
#'     tree_structure()
#'  
#'   corpus("GERMAPARL2MINI") %>%
#'     subset(speaker_name == "Konrad Adenauer") %>%
#'     subset(p) %>%
#'     tree_structure()
#'   
#'   corpus("GERMAPARL2MINI") %>%
#'     subset(ne_type) %>%
#'     tree_structure()
#' }
#' @rdname tree_structure
setMethod("tree_structure", "subcorpus", function(x){
  tree_structure(
    x = as(x, "corpus"),
    s_attribute = x@s_attribute_strucs
  )
})

#' @rdname tree_structure
setMethod("tree_structure", "corpus", function(x, s_attribute = NULL){
  xml_file <- fs::path(slot(x, "data_dir"), "sample.xml")
  if (!file.exists(xml_file)){
    message("file sample.xml not present in corpus home directory")
    return(invisible(NULL))
  }
  xml_doc <- xml2::read_xml(xml_file)
  tree_structure(xml_doc, s_attribute = s_attribute)
})
