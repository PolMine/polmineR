#' Initialize a partition
#' 
#' Set up an object of the partition class. Frequency lists are computeted if a pAttribute is provided.
#' 
#' The function sets up a partition based on a list of s-attributes with respective values.
#' The s-attributes defining the partition are a list, e.g. list(text_type="speech", text_year="2013").
#' The values of the list may contain regular expressions. To use regular expression syntex, set the 
#' parameter regex to \code{"TRUE"}. Regular expressions are passed into grep, i.e. the regex syntax
#' used in R needs to be used (double backlashes etc.).
#' 
#' @param object character-vector - the CWB-corpus to be used
#' @param def list consisting of a set of character vectors (see
#' details)
#' @param name name of the new partition, defaults to "noName"
#' @param encoding encoding of the corpus (typically "LATIN1 or "(UTF-8)), if NULL, the encoding provided in the registry file of the corpus (charset="...") will be used b
#' @param pAttribute the pAttribute(s) for which term frequencies shall be retrieved
#' @param meta a character vector
#' @param regex logical (defaults to FALSE), if TRUE, the s-attributes provided will be handeled as regular expressions; the length of the character vectors with s-attributes then needs to be 1
#' @param xml either 'flat' (default) or 'nested'
#' @param id2str whether to turn token ids to strings (set FALSE to minimize object.size / memory consumption)
#' @param type character vector (length 1) specifying the type of corpus / partition (e.g. "plpr")
#' @param mc whether to use multicore (for counting terms)
#' @param verbose logical, defaults to TRUE
#' @param ... parameters passed into the partition-method
#' @return An object of the S4 class 'partition'
#' @author Andreas Blaette
#' @examples
#' use(polmineR.sampleCorpus)
#' spd <- partition("PLPRBTTXT", def=list(text_party="SPD", text_type="speech"))
#' kauder <- partition("PLPRBTTXT", def=list(text_name="Volker Kauder"), pAttribute=c("word"))
#' merkel <- partition("PLPRBTTXT", list(text_name=".*Merkel"), pAttribute="word", regex=TRUE)
#' sAttributes(merkel, "text_date")
#' sAttributes(merkel, "text_name")
#' merkel <- partition("PLPRBTTXT", list(text_name="Angela Dorothea Merkel", text_date="2009-11-10", text_type="speech"), pAttribute="word")
#' merkel <- subset(merkel, !word %in% punctuation)
#' merkel <- subset(merkel, !word %in% tm::stopwords("de"))
#' @import methods
#' @exportMethod partition
#' @rdname partition
#' @aliases partition
setGeneric("partition", function(object, ...){standardGeneric("partition")})

#' @rdname partition
setMethod("partition", "character", function(
  object, def=NULL, name=c(""),
  encoding=NULL, pAttribute=NULL, meta=NULL, regex=FALSE, xml="flat", id2str=TRUE, type=NULL,
  mc=FALSE, verbose=TRUE
) {
  corpus <- object
  if (!corpus %in% cqi_list_corpora()) warning("corpus is not an available CWB corpus")
  if (verbose==TRUE) message('Setting up partition ', name)
  if (is.null(type)){
    parsedRegistry <- parseRegistry(object)
    if (!"type" %in% names(parsedRegistry)){
      Partition <- new('partition', stat=data.table())    
    } else {
      type <- parsedRegistry[["type"]]
      if (verbose == TRUE) message("... type of the corpus is ", type)
      if (type %in% c("press", "plpr")) assign("Partition", new(paste(type, "Partition", sep="")))  
    }
  } else {
    if (type %in% c("press", "plpr")) Partition <- new(paste(type, "Partition", sep=""))
  }
  Partition@call <- deparse(match.call())
  if ((corpus %in% cqi_list_corpora()) == FALSE) warning("corpus not in registry - maybe a typo?")
  Partition@corpus <- corpus
  if(is.null(def)){
    parsedInfo <- parseInfoFile(object)
    if ("ANCHOR_ELEMENT" %in% names(parsedInfo)){
      def <- list()
      def[[parsedInfo["ANCHOR_ELEMENT"]]] <- ".*"
      regex <- TRUE
    } else {
      message("... no idea what the anchor element might be, please provide explicitly")
    }
  }
  if(is.null(encoding)) {
    Partition@encoding <- .getCorpusEncoding(Partition@corpus)  
  } else {
    Partition@encoding <- encoding
  }
  if (verbose==TRUE) message('... encoding of the corpus is ', Partition@encoding)
  Partition@name <- name
  Partition@sAttributes <- lapply(def, function(x).adjustEncoding(x, Partition@encoding))  
  Partition@sAttributeStrucs <- names(def)[length(def)]
  Partition@xml <- xml
  if (verbose==TRUE) message('... computing corpus positions and retrieving strucs')
  if (xml=="flat") {
    Partition <- .flatXmlSattributes2cpos(Partition, regex)
  } else if (xml=="nested") {
    Partition <- .nestedXmlSattributes2cpos(Partition, regex)
  } else {
    warning("WARNING: Value of 'xml' is not valid!")
  }
  if (!is.null(Partition)) {
    if (verbose==TRUE) message('... computing partition size')
    Partition@size <- size(Partition)
    if (!is.null(pAttribute)) if (pAttribute[1] == FALSE) {pAttribute <- NULL}
    if (!is.null(pAttribute)) {
      stopifnot(is.character(pAttribute) == TRUE, length(pAttribute) <= 2, all(pAttribute %in% pAttributes(object)))
      if (verbose==TRUE) message('... computing term frequencies (for p-attribute ', paste(pAttribute, collapse=", "), ')')  
      Partition@stat <- getTermFrequencies(.Object=Partition, pAttribute=pAttribute, id2str=id2str, mc=mc)
      Partition@pAttribute <- pAttribute
      Partition <- sort(Partition, "count")
    }
    if (!is.null(meta)) {
      if (verbose==TRUE) message('... setting up metadata (table and list of values)')
      Partition@metadata <- meta(Partition, sAttributes=meta)
    }
    if (verbose==TRUE) message('... partition is set up\n')
  } else {
    message("... setting up the partition failed (returning NULL object)")
  }
  Partition
})


#' @rdname partition
setMethod("partition", "list", function(
  object, name=c(""), encoding=NULL, pAttribute=NULL, meta=NULL,
  regex=FALSE, xml="flat", id2str=TRUE, type=NULL, mc=FALSE, verbose=TRUE
) {
  partition(
    object=get('session', '.GlobalEnv')@corpus,
    def=object, name=name, encoding=encoding, pAttribute=pAttribute,
    meta=meta, regex=regex, xml=xml, id2str=id2str, type=type, mc=mc, verbose=verbose
    )
})


#' Get the cpos for a partition based on sattributes
#' 
#' Augment the partition object
#' 
#' The function works nicely - potentially, it can be optimized, but I have tried many things.
#' Interestingly, the for-loop is more effective than a vectorized version
#' @param partition the rudimentary partition object
#' @return an augmented partition object (includes now cpos and strucs)
#' @noRd
.flatXmlSattributes2cpos <- function(part, regex){
  root <- paste(part@corpus, '.', part@sAttributeStrucs, sep='')
  meta <- data.frame(struc=c(0:(cqi_attribute_size(root)-1)), select=rep(0, times=cqi_attribute_size(root)))
  if (length(part@sAttributes) > 0) {
    for (s in names(part@sAttributes)){
      sattr <- paste(part@corpus, ".", s, sep="")
      meta[,2] <- as.vector(cqi_struc2str(sattr, meta[,1]))
      Encoding(meta[,2]) <- part@encoding
      if (regex==FALSE) {
        meta <- meta[which(meta[,2] %in% part@sAttributes[[s]]),]
      } else {
        lines <- lapply(part@sAttributes[[s]], function(x) grep(x, meta[,2]))
        meta <- meta[unique(unlist(lines)),]
      }
    }
    if (nrow(meta) == 0) {
      warning(paste("no strucs found for the values provided for s-attribute", s))
    }
  }
  if (nrow(meta) != 0){
    part@cpos <- matrix(
      data=unlist(lapply(meta[,1], function(x)cqi_struc2cpos(root, x))),
      ncol=2, byrow=TRUE
    )
    part@strucs <- as.numeric(meta[,1])
  } else {
    warning("returning a NULL object")
    part <- NULL    
  }
  part
}

#' Get the cpos for a partition based on sattributes
#' 
#' Augment the partition object by strucs and cpos
#' 
#' @param partition the rudimentary partition object
#' @return an augmented partition object (includes now cpos and strucs)
#' @noRd
.nestedXmlSattributes2cpos <- function(Partition, regex){
  sAttr <- vapply(
    names(Partition@sAttributes),
    USE.NAMES=TRUE, FUN.VALUE="character",
    function(x)paste(Partition@corpus, '.', x, sep='')
  )
  sAttr <- rev(sAttr)
  strucs <- c(0:(cqi_attribute_size(sAttr[1])-1))
  cpos <- matrix(
    unlist(lapply(strucs,
                  function(x) cqi_struc2cpos(sAttr[1], x)
    )
    ), byrow=TRUE, ncol=2
  )
  for (i in c(1:length(sAttr))){
    if ( i == 1) {
      meta <- cqi_struc2str(sAttr[i], strucs)
    } else if ( i > 1 ) {
      meta <- cqi_struc2str(sAttr[i], cqi_cpos2struc(sAttr[i], cpos[,1]))
    }
    Encoding(meta) <- Partition@encoding
    if (regex == FALSE) {
      hits <- which(meta %in% Partition@sAttributes[[names(sAttr)[i]]])
    } else if (regex == TRUE) {
      # hits <- grep(Partition@sAttributes[[names(sAttr)[i]]], meta)
      hits <- unique(unlist(lapply(Partition@sAttributes[[names(sAttr)[i]]], function(x) grep(x, meta[,2]))))
    }
    cpos <- cpos[hits,]
    strucs <- strucs[hits]
  }
  Partition@strucs <- strucs
  Partition@cpos <- cpos
  Partition
}


#' @rdname partition
setMethod("partition", "session", function(object){
  .getClassObjectsAvailable(".GlobalEnv", "partition")
})


#' @rdname partition
setMethod("partition", "partition", function(object, def, name=c(""), regex=FALSE, pAttribute=NULL, id2str=TRUE, type=NULL, verbose=TRUE, mc=FALSE, ...){
  newPartition <- new(
    class(object)[1],
    corpus=object@corpus, encoding=object@encoding, name=name, xml=object@xml,
    stat=data.table()
    )
  if (verbose == TRUE) message('Setting up partition', name)
  def <- lapply(def, function(x) .adjustEncoding(x, object@encoding))  
  newPartition@sAttributes <- c(object@sAttributes, def)
  newPartition@sAttributeStrucs <- names(newPartition@sAttributes)[length(newPartition@sAttributes)]
  if (verbose == TRUE) message('... getting strucs and corpus positions')
  newPartition <- .zoomingSattributes2cpos(object, newPartition, def, regex)
  newPartition@size <- size(newPartition)
  if (length(pAttribute)>0) {
    newPartition@stat <- getTermFrequencies(.Object=newPartition, pAttribute=pAttribute, id2str=id2str, mc=mc)
    newPartition@pAttribute <- pAttribute
  }
  if (verbose == TRUE) message('... partition is set up')
  newPartition
})

#' Augment the partition object by strucs and cpos
#' 
#' @param Partition the partition object to be specified
#' @param newPartition the new partition
#' @param sAttribute info for specification (a list)
#' @param method either "in" or "grep"
#' @return an augmented partition object
#' @noRd
.zoomingSattributes2cpos <- function(Partition, newPartition, sAttribute, regex){
  sAttr <- paste(Partition@corpus, '.', names(sAttribute), sep='')
  if (Partition@xml == "flat") {
    str <- cqi_struc2str(sAttr, Partition@strucs)    
  } else if (Partition@xml == "nested") {
    str <- cqi_struc2str(sAttr, cqi_cpos2struc(sAttr, Partition@cpos[,1]))    
  }
  Encoding(str) <- newPartition@encoding
  if (regex == FALSE) {
    hits <- which(str %in% sAttribute[[1]])
  } else if (regex == TRUE) {
    hits <- grep(sAttribute[[1]], str)
  }
  newCpos <- Partition@cpos[hits,]
  if (class(newCpos) == "matrix"){
    newPartition@cpos <- newCpos
  } else if (class(newCpos) == "integer") {
    newPartition@cpos <- matrix(newCpos, ncol=2, byrow=TRUE)     
  }
  newPartition@strucs <- Partition@strucs[hits]
  if (length(Partition@metadata) == 2) {
    message('... adjusting metadata')
    newPartition@metadata <- Partition@metadata[hits,]
    # newPartition <- .partitionMetadataValues(newPartition)
  }
  newPartition
}


#' @rdname partition
setMethod("partition", "missing", function() {
  if (requireNamespace("shiny", quietly=TRUE) && requireNamespace("miniUI", quietly=TRUE)){
    ui <- miniUI::miniPage(
      shiny::tags$head(shiny::tags$style(shiny::HTML("
                                .form-group, .form-control, .selectize-input, .selectize-dropdown, .radio-inline{
                                font-size: 90%;
                                }
                                "))),
      miniUI::gadgetTitleBar(
        "make partition",
        right = miniUI::miniTitleBarButton("done", "Generate", primary = TRUE)
      ),
      miniUI::miniContentPanel(
        shiny::textInput(inputId = "name", label = "Partition name:", value = "FOO"),
        shiny::selectInput("corpus", "Corpus:", choices = cqi_list_corpora(), selected = "PLPRTXT"),
        shiny::textInput(inputId = "def", label = "sAttributes:", value = 'text_year="2012"'),
        shiny::selectInput(inputId="pAttribute", label="pAttribute:", multiple = TRUE, choices=list(none = "", word = "word", lemma = "lemma")),
        shiny::radioButtons("regex", "Use regular expressions:", choices = list("TRUE", "FALSE"), inline = TRUE),
        shiny::radioButtons("xml", "XML type:", choices = list("flat", "nested"), inline = TRUE)
      ),
      
      miniUI::miniButtonBlock(
        shiny::actionButton("check", " check", icon=shiny::icon("stethoscope")),
        shiny::actionButton("save", " save", icon = shiny::icon("save"))
      )
      )
    
    server <- function(input, output, session) {
      shiny::observeEvent(input$done, {
        shiny::stopApp(
          partition(
            as.character(input$corpus),
            def=eval(parse(text=paste("list(", input$def, ")", sep=""))),
            name=input$name,
            pAttribute=input$pAttribute,
            regex=input$regex,
            xml=input$xml,
            mc=input$mc,
            verbose=FALSE
          )
        )
      })
      shiny::observeEvent(input$check, {
        print("yeah")
      })
      
    }
    shiny::runGadget(ui, server)
  }
})

#' @export partitionGadget
#' @rdname partition
partitionGadget <- function() partition()
