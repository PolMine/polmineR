#' trim an object
#' 
#' Method that includes varying to adjust objects from the driller package by 
#' applying thresholds, minimum frequencies etc. It can be applied to 'context',
#' 'keyness', 'context', 'partition' and 'partitionCluster' objects. See 
#' the respective documentation:
#' \describe{
#'  \item{context:}{\code{method?trim("context")}}
#'  \item{keyness:}{\code{method?trim("keyness")}} 
#'  \item{partition:}{\code{method?trim("partition")}}
#'  \item{partitionCluster:}{\code{method?trim("partitionCluster")}} 
#'  \item{crosstab:}{\code{method?trim("crosstab")}}
#' }
#' 
#' @param object the object to be trimmed
#' @param ... further arguments
#' @author Andreas Blaette
#' @docType methods
#' @aliases trim trim-method
#' @rdname trim-method
setGeneric("trim", function(object, ...){standardGeneric("trim")})

#' add pos information
#' 
#' Add information on part-of-speech tags of tokens to tokens. The method is 
#' available for objects of the classes 'partition', 'partitionCluster' and
#' 'keyness' respectively. 
#' \code{method?addPos("partitionCluster")}, \code{method?addPos("keyness")}).
#' @param object either a partition, a partitionCluster or a keyness object
#' @param ... further arguments
#' @return the original, enhanced object
#' @docType methods
setGeneric("addPos", function(object,...){standardGeneric("addPos")})




#' contextual analysis
#' 
#' Statistical analysis of the context of a token. The method can be applied to partition or partitionCluster class objects.
#' \describe{
#'  \item{partition:}{\code{method?context("partition")}}
#'  \item{partitionCluster:}{\code{method?trim("partitionCluster")}} 
#' }


#' get term frequencies
#' 
#' Method to obtain term frequencies for one or multiple terms or queries.
#' The method can be applied to partition or partitionCluster class objects.
#' If object is a character string, frequencies for a whole corpus are returned.
#' Please see the respective documentation for details 
#' (\code{method?tf("partition")}, \code{method?tf("partitionCluster")} or
#' \code{method?tf("character")}).
#' 
#' @param object either a partition or a partitionCluster object
#' @param ... further parameters
#' @aliases tf tf-method
#' @rdname tf-method
setGeneric("tf", function(object, ...){standardGeneric("tf")})

#' mail result
#' 
#' Mail a result (to yourself).
#' Please see the respective documentation for details 
#' (\code{method?mail("keyness")}, \code{method?mail("partition")}.
#' 
#' @param object a driller object
#' @param ... further parameters
#' @aliases mail mail-method
#' @rdname mail
setGeneric("mail", function(object, ...){standardGeneric("mail")})

#' mail a result
#' 
#' still experimental
#' 
#' @param object an object with some statistics
#' @param to the receiver of the mail message
#' @param filename name of the file to be sent out
#' @param what what to send (defaults to "html")
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @include statistics.R
#' @name mail,partition-method
#' @rdname mail-partition-method
#' @aliases mail,partition-method
#' @docType methods
setMethod("mail", "partition", function(object, to=NULL, filename="drillerExport.html", what="html"){
  msg <- list('Delivering something to read.\nSincerely yours\nThe driller\n')
  filename <- html(
    object, meta=NULL, browser=FALSE,
    filename=file.path(tempdir(), filename)
  )
  msg[[length(msg)+1]] <- mime_part(filename)
  status <- .mail(msg, to)
  status$msg
})




#' mail result of context analysis
#' 
#' still experimental
#' 
#' @param object a context object
#' @param to the receiver of the mail message
#' @param nrow the number of rows
#' @param fileFormat either csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @include statistics.R
#' @name context-keyness-method
#' @rdname context-mail-method
#' @aliases mail,context-method
#' @docType methods
setMethod("mail", "context", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if(is.null(nrow)) nrow <- nrow(object@stat)
  msg <- list('Delivering Tables.\nSincerely yours\nThe driller\n')
  msg <- .attachTables(object@stat, nrow, msg, "keyness", fileFormat)
  status <- .mail(msg, to)
  status$msg
})


#' mail result of keyness analysis
#' 
#' still experimental
#' 
#' @param object an object with some statistics
#' @param to the receiver of the mail message
#' @param nrow the number of rows
#' @param fileFormat either csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @include statistics.R
#' @name mail-keyness-method
#' @rdname keyness-mail-method
#' @aliases mail,keyness-method
#' @include keyness.R
#' @docType methods
setMethod("mail", "keyness", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  if(is.null(nrow)) nrow <- nrow(object@stat)
  msg <- list('Delivering Tables.\nSincerely yours\nThe driller\n')
  msg <- .attachTables(object@stat, nrow, msg, "keyness", fileFormat)
  status <- .mail(msg, to)
  status$msg
})




setGeneric("sAttributes", function(object,...){standardGeneric("sAttributes")})

#' enrich an object
#' 
#' Method to fill slots of a partition, partitionCluster or keyness object that 
#' have not been set up previously. See the respective documentation:
#' \describe{
#'  \item{partition:}{\code{method?enrich("partition")}}
#'  \item{partitionCluster:}{\code{method?enrich("partitionCluster")}}
#'  \item{keyness:}{\code{method?enrich("keyness")}}
#' }
#' 
#' @param object a partition, partitionCluster or keyness object
#' @param ... further parameters
#' @aliases enrich enrich-method
#' @docType methods
#' @rdname enrich-method
setGeneric("enrich", function(object, ...){standardGeneric("enrich")})

setGeneric("html", function(object, ...){standardGeneric("html")})

#' Convert partition/partition Cluster into html
#' 
#' A partition is converted into a html file and displayed in a browser. If a partitionCluster
#' is provided, the browser will open several windows.
#'   
#' @param object a partition object
#' @param meta metadata for output
#' @param browser logical (defaults to TRUE), whether to direct output to browser, if FALSE, the generated html will be returned
#' @param filename filename for the html file, if NULL (default), a temporary file is created
#' @param type the type of html to be generated
#' @include partition.R methods.R
#' @rdname html
#' @exportMethod html
#' @aliases html html-method html,partition-method html,partitionCluster-method
setMethod("html", "partition", function(object, meta=NULL, browser=TRUE, filename=NULL, type="debate"){
  if (is.null(meta)) meta <- get("drillingControls", '.GlobalEnv')[['metadata']]
  object <- enrich(object, meta=meta)
  markdown <- .partition2markdown(object, type)
  markdown <- paste(
    paste('## Excerpt from corpus', object@corpus, '\n* * *\n'),
    markdown,
    '\n* * *\n',
    collapse="\n")
  if (is.null(filename)) {
    htmlFile <- .markdown2tmpfile(markdown)
  } else {
    cat(markdown, file=filename)
    htmlFile <- filename
  }
  if (browser == TRUE){
    browseURL(htmlFile)
    retval <- c("[html output redirected to browser]")
  } else if (browser == FALSE) {
    retval <- htmlFile
  }
  retval
})


setGeneric("as.sparseMatrix", function(x,...){standardGeneric("as.sparseMatrix")})

setGeneric("as.partitionCluster", function(object,...){standardGeneric("as.partitionCluster")})

setGeneric("as.TermContextMatrix", function(x, col, ...) {standardGeneric("as.TermContextMatrix")})

setGeneric("score", function(object, ...){standardGeneric("score")})

setGeneric("speeches", function(object, ...){standardGeneric("speeches")})

setGeneric("kwic", function(object, ...){standardGeneric("kwic")})


#' Transform a context cluster into a Term Context Matrix
#' 
#' Method based on the tm package, adds to as.TermDocumentMatrix
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a contextCluster object (S3 class)
#' @param col the col of the stat table to take
#' @param ... to make the check happy
#' @method as.TermContextMatrix contextCluster
#' @return a TermContextMatrix
#' @author Andreas Blaette
#' @docType method
#' @importFrom slam simple_triplet_matrix
#' @exportMethod as.TermContextMatrix
#' @noRd
setMethod("as.TermContextMatrix", "contextCluster", function (x, col, ...) {
  encoding <- unique(unlist(lapply(x@contexts, function(c) c@encoding)))
  corpus <- unique(unlist(lapply(x@contexts, function(c) c@corpus)))
  pAttribute <- unique(unlist(lapply(x@contexts, function(c) c@pattribute)))
  pAttr <- paste(corpus, '.', pAttribute, sep='')
  i <- unlist(lapply(x@contexts, function(c) (cqi_str2id(pAttr, rownames(c@stat))+1)))
  j <- unlist(lapply(c(1:length(x@contexts)), function(m) {rep(m,times=nrow(x[[m]]@stat))}))
  v <- unlist(lapply(x@contexts, function(c) c@stat[,col]))
  lexiconSize <- cqi_lexicon_size(pAttr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x@contexts),
                               nrow=lexiconSize+1,
                               dimnames=list(
                                 Terms=cqi_id2str(pAttr, c(0:lexiconSize)),
                                 Docs=names(x@contexts))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
  class(mat) <- c("TermContextMatrix", "TermDocumentMatrix", "simple_triplet_matrix")
  mat
})

#' Fill slot 'pos' of partition (or partitionCluster) object
#' 
#' The 'pos'-slot of the partition (or partitionCluster) object is filled with tables
#' providing a statistic on the frequency of a pos-tag of a token
#' in the partition.
#' 
#' @param object a partition class object
#' @param pAttribute character vector (typically c("word") or c("lemma") or c("word", "lemma"))
#' @return an augmented partition or partitionCluster object (includes pos now)
#' @author Andreas Blaette
#' @exportMethod addPos
#' @noRd
setMethod("addPos", "partition", function(object, pAttribute){
  if (length(pAttribute) > 1) warning("taking only one pAttribute at a time")
  message("Adding pos information to partition object ", object@label)
  cpos <- unlist(apply(object@cpos, 1, function(x) c(x[1]:x[2])))
  message("... retrieving corpus information")
  bag <- data.frame(
    token=cqi_cpos2id(paste(object@corpus, '.', pAttribute, sep=''), cpos),
    pos=cqi_cpos2id(paste(object@corpus, '.pos', sep=''), cpos)
  )
  message("... doing the calculations")
  object@pos[[pAttribute]] <- list()
  crosstab <- table(bag)
  rownames(crosstab) <- cqi_id2str(paste(object@corpus, '.', pAttribute, sep=''), as.integer(rownames(crosstab)))
  colnames(crosstab) <- cqi_id2str(paste(object@corpus, '.pos', sep=''), as.integer(colnames(crosstab)))
  object@pos[[pAttribute]] <- apply(crosstab, 1, function(x) colnames(crosstab)[which.max(x)])
  Encoding(names(object@pos[[pAttribute]])) <- object@encoding
  # to make sure that there are no superfluous pos information
  object@pos[[pAttribute]] <- object@pos[[pAttribute]][names(object@pos[[pAttribute]]) %in% rownames(object@tf[[pAttribute]])]
  object
})

#' Fill slot 'pos' of a partitionCluster object with tables giving the statistic of pos
#' 
#' Augment the partitionCluster object
#' 
#' @param object a partition class object
#' @param pAttribute character vector - pos statistic for lemma or word
#' @return an augmented partition object (includes pos now)
#' @author Andreas Blaette
#' @noRd
setMethod("addPos", "partitionCluster", function(object, pAttribute){
  pimpedCluster <- object
  if (get('drillingControls', '.GlobalEnv')[['multicore']] == TRUE) {
    pimpedCluster@partitions <- mclapply(object@partitions, function(x) addPos(x, pAttribute))
  } else {
    pimpedCluster@partitions <- lapply(object@partitions, function(x) addPos(x, pAttribute))    
  }
  pimpedCluster
})

#' supplement keyness object with pos information
#' 
#' A keyness object will be supplemented with pos information. The working of the
#' method is potentially slow. It is recommended to trim the object first, before
#' adding pos information.
#' 
#' @param object the keyness object
#' @param Partition a partition object (the corpus of interest)
#' @return an enhanced keyness object 
#' @noRd 
setMethod("addPos", "keyness",
          function(object, Partition=NULL){
            if (is.null(Partition)){
              object <- .addPos(object)
            } else if (class(Partition) == "partition"){
              if (object@pattribute %in% names(Partition@pos)) {
                pos <- vapply(
                  rownames(object@stat[1:50, ]),
                  function(x) return(Partition@pos[[object@pattribute]][["max"]][x]),
                  USE.NAMES=FALSE,
                  FUN.VALUE="character")
                object@stat <- cbind(object@stat, pos=pos)
              }
            }
            object 
          }
)

#' enrich partition object (S4 method)
#' 
#' Fill slots of a partition object: Add object size, metainformation, pos.
#' 
#' @param object a partition object
#' @param size logical, defaults to FALSE
#' @param tf character vector providing the p-attributes for which frequency
#'   tables shall be generated
#' @param meta character vector providing s-attributes for which metainformation shall be supplied
#' @param addPos character vector providing p-attributes 
#' @param verbose logical, defaults to TRUE   
#' @exportMethod enrich
#' @aliases enrich,partition-method
#' @docType methods
#' @rdname enrich-partition-method
setMethod("enrich", "partition", function(object, size=FALSE, tf=NULL, meta=NULL, addPos=NULL, verbose=TRUE){
  if (size == TRUE) object@size <- .partition.size(object)
  if (length(tf>0)) {
    for (what in tf){
      if (verbose==TRUE) message('... computing term frequencies (for p-attribute ', what, ')')  
      object@tf[[what]] <- .cpos2tf(object, what)
    }
  }
  if (!is.null(meta)) {
    if (verbose==TRUE) message('... setting up metadata (table and list of values)')
    object <- .partition.metadata(object, meta)
  }
  if(!is.null(addPos)){
    object <- addPos(object, addPos)
  }
  object
})

#' enrich partition object (S4 method)
#' 
#' Fill slots of a partition object: Add object size, metainformation, pos.
#' 
#' @param object a partition object
#' @param size logical, defaults to FALSE
#' @param tf character vector providing the p-attributes for which frequency
#'   tables shall be generated
#' @param meta character vector providing s-attributes for which metainformation shall be supplied
#' @param addPos character vector providing p-attributes
#' @param mc logical whether to use multicore parallelization
#' @param verbose logical, defaults to TRUE   
#' @exportMethod enrich
#' @aliases enrich,partitionCluster-method
#' @docType methods
#' @rdname enrich-partitionCluster-method
setMethod("enrich", "partitionCluster", function(object, size=TRUE, tf=c(), meta=NULL, addPos=NULL, mc=FALSE, verbose=TRUE){
  if (mc == FALSE) {
    object@partitions <- lapply(
      object@partitions,
      function(p) enrich(p, size=size, tf=tf, meta=meta, addPos, verbose=TRUE)
    )
  } else if (mc == TRUE){
    object@partitions <- mclapply(
      object@partitions,
      function(p) enrich(p, size=size, tf=tf, meta=meta, addPos, verbose=TRUE)
    )    
  }
  object
})


#' Enrich keyness object
#' 
#' Wrapper for adding a statistic on pos attributes to keyness object
#' 
#' @param object a keyness object
#' @param addPos logical, whether to add POS information to keyness object
#' @param verbose whether R should be talkative
#' @exportMethod enrich
#' @docType methods
#' @rdname enrich-keyness-method
#' @aliases enrich,keyness-method
setMethod("enrich", "keyness", function(object, addPos=NULL, verbose=TRUE){
  if (addPos==TRUE) object <- addPos(object, Partition=NULL)
  object
})

setMethod("enrich", "keynessCluster", function(object, addPos=NULL, verbose=TRUE, mc=NULL){
  if (is.null(mc)) mc <- get("drillingControls", '.GlobalEnv')[['multicore']]
  rework <- new("keynessCluster")
  if (mc==FALSE){
  rework@objects <- lapply(
    setNames(object@objects, names(object@objects)),
    function(x) enrich(x, addPos=addPos, verbose=TRUE)
    )
  } else if (mc==TRUE){
    rework@objects <- mclapply(
      setNames(object@objects, names(object@objects)),
      function(x) enrich(x, addPos=addPos, verbose=TRUE)
    )    
  }
  rework
})


#' Get statistics table from an object
#' 
#' @param x object with a statistics table
#' @param ... any further arguments
#' @rdname asDataFrame-method
#' @name as.data.frame
#' @aliases as.data.frame,keyness-method as.data.frame,context-method
#' @exportMethod as.data.frame
setMethod("as.data.frame", "keyness", function(x, ...) x@stat )
setMethod("as.data.frame", "context", function(x, ...) x@stat )

#' Basic information on an object
#' 
#' @param x the object to learn about
#' @exportMethod info
#' @docType methods
#' @rdname info-method
#' @aliases info info,keyness-method
setGeneric("info", function(x){standardGeneric("info")})

#' Information on a keyness object
#' 
#' @exportMethod info
#' @noRd
setMethod(
  "info", "keyness",
  function(x){
    cat("the statistics table has", nrow(x@stat), "rows\n")
    cat("pos attributest have been added: ")
    if ("pos" %in% colnames(x@stat)){
      cat("YES\n")
    } else {
      cat("NO\n")
    }
  })

#' @include context.R partition.R
#' @exportMethod kwic
setMethod("kwic", "context", function(object, metadata=NULL, collocate=c()){
  .kwic(ctxt=object, metadata=metadata, collocate=collocate)
})


#' KWIC output
#' 
#' Prepare and show 'keyword in context' (KWIC). The same result can be achieved by 
#' applying the kwich method on either a partition or a context object.
#' 
#' @param object a partition object
#' @param query what to look up
#' @param leftContext to the left
#' @param rightContext to the right
#' @param meta metainformation to display
#' @param pAttribute typically 'word' or 'lemma'
#' @param collocate only show kwic if a certain word is present
#' @aliases kwic,partition-method show,kwic-method kwic,context-method kwic
#' @examples
#' bt <- partition("PLPRTXT", def=list(text_date=".*"), method="grep")
#' foo <- kwic(bt, "Integration")
#' foo <- kwic(bt, "Integration", leftContext=20, rightContext=20, meta=c("text_date", "text_name", "text_party")) 
#' @exportMethod kwic
setMethod("kwic", "partition", function(
  object, query,
  leftContext=0,
  rightContext=0,
  meta=NULL,
  pAttribute="word",
  collocate=c()
){
  ctxt <- context(
    object=object, query=query, pAttribute=pAttribute,
    leftContext=leftContext, rightContext=rightContext,
    statisticalTest=NULL
  )
  .kwic(ctxt=ctxt, metadata=meta, collocate=collocate)
})


#' mail kwic/concordances
#' 
#' still experimental
#' 
#' @param object the concordance object
#' @param to the receiver of the mail message
#' @param nrow the number of rows of the table (if NULL, the whole table will be sent)
#' @param fileFormat csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @include methods.R
#' @name mail-kwic-method
#' @rdname mail-kwic-method
#' @aliases mail,kwic-method
#' @docType methods
setMethod("mail", "kwic", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  msg <- list('Delivering kwic.\nSincerely yours\nThe driller\n')
  if(is.null(nrow)) nrow <- nrow(object@stat)
  msg <- .attachTables(object@table, nrow, msg, "kwic", fileFormat) 
  status <- .mail(msg, to)
  status$msg
})

#' mail crosstab
#' 
#' For exporting.
#' 
#' @param object the crosstab object
#' @param to the receiver of the mail message
#' @param nrow the number of rows of the table (if NULL, the whole table will be sent)
#' @param fileFormat csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @include dispersion.R
#' @name mail-crosstab-method
#' @rdname mail-crosstab-method
#' @aliases mail,crosstab-method
#' @docType methods
setMethod("mail", "crosstab", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  msg <- list('Delivering a crosstabulation.\nSincerely yours\nThe driller\n')
  if(is.null(nrow)) nrow <- nrow(object@abs)
  msg <- .attachTables(foo@abs, nrow, msg, "crosstabAbs", fileFormat) 
  msg <- .attachTables(foo@rel, nrow, msg, "crosstabRel", fileFormat) 
  status <- .mail(msg, to)
  status$msg
})

#' mail a data frame
#' 
#' For exporting.
#' 
#' @param object the data frame
#' @param to the receiver of the mail message
#' @param nrow the number of rows of the table (if NULL, the whole table will be sent)
#' @param fileFormat csv or xlsx, or both
#' @exportMethod mail
#' @importFrom sendmailR sendmail sendmail_options
#' @importFrom xlsx write.xlsx
#' @include dispersion.R
#' @name mail-data.frame-method
#' @rdname mail-data.frame-method
#' @aliases mail,data.frame-method
#' @docType methods
setMethod("mail", "data.frame", function(object, to=NULL, nrow=NULL, fileFormat=c("csv", "xlsx")){
  msg <- list('Delivering a data frame.\nSincerely yours\nThe driller\n')
  if(is.null(nrow)) nrow <- nrow(object)
  msg <- .attachTables(object, nrow, msg, "dataFrame", fileFormat) 
  status <- .mail(msg, to)
  status$msg
})

#'@include partition.R
#' @exportMethod as.partitionCluster
setMethod("as.partitionCluster", "partition", function(object){
  newCluster <- new("partitionCluster")
  newCluster@partitions[[1]] <- object
  names(newCluster@partitions)[1] <- object@label
  newCluster@corpus <- object@corpus
  newCluster@encoding <- object@encoding
  newCluster@explanation <- c("derived from a partition object")
  newCluster
})

setMethod("as.partitionCluster", "list", function(object, ...){
  if (!all(unlist(lapply(object, class))=="partition")) warning("all objects in list need to be partition objects")
  newCluster <- new("partitionCluster")
  newCluster@partitions <- object
  newCluster@corpus <- unique(unlist(lapply(newCluster@partitions, function(x) x@corpus)))
  newCluster@encoding <- unique(unlist(lapply(newCluster@partitions, function(x) x@encoding)))
  names(newCluster@partitions) <- vapply(newCluster@partitions, function(x) x@label, FUN.VALUE="character")
  newCluster
})

setMethod("html", "partitionCluster", function(object, filename=c(), type="debate"){
  markdown <- paste(lapply(object@partitions, function(p) .partition2markdown(p, type)), collapse="\n* * *\n")
  markdown <- paste(
    paste('## Excerpt from corpus', object@corpus, '\n* * *\n'),
    markdown,
    '\n* * *\n',
    collapse="\n")
  if (is.null(filename)) {
    htmlFile <- .markdown2tmpfile(markdown)
  } else {
    cat(markdown, file=filename)    
  }
  if (is.null(filename)) browseURL(htmlFile)
})

