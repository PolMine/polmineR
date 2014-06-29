#' @include partition.R
NULL

.partition2markdown <- function(object, type="speech"){
  if (length(object@strucs)>1){
    s <- object@strucs
    gapSize <- s[2:length(s)] - s[1:(length(s)-1)]
    gap <- c(0, vapply(gapSize, FUN.VALUE=1, function(x) ifelse(x >1, 1, 0) ))
    m <- object@metadata$table
    metaNo <- ncol(m)
    metaComp <- data.frame(m[2:nrow(m),], m[1:(nrow(m)-1),])
    metaChange <- !apply(
      metaComp, 1,
      function(x) all(x[1:metaNo] == x[(metaNo+1):length(x)])
    )
    metaChange <- c(TRUE, metaChange)
    metadata <- apply(object@metadata$table, 2, function(x) as.vector(x))
  } else {
    gap <- 0
    metaChange <- TRUE
    metadata <- matrix(apply(object@metadata$table, 2, function(x) as.vector(x)), nrow=1)
  }
  type <- cqi_struc2str(paste(object@corpus, ".text_type", sep=""), object@strucs)
  markdown <- sapply(c(1:nrow(metadata)), function(i) {
    meta <- c("")
    if (metaChange[i] == TRUE) { 
      meta <- paste(metadata[i,], collapse=" | ", sep="")
      meta <- paste("\n###", meta, "\n", collapse="")
    }
    plainText <- paste(
      cqi_cpos2str(paste(
        object@corpus, '.word', sep=''),
        c(object@cpos[i,1]:object@cpos[i, 2])),
      collapse=" "
    )
    if (type[i] == "interjection") plainText <- paste("\n> ", plainText, "\n", sep="")
    return(paste(meta, plainText))
  })
  markdown <- paste(markdown, collapse="\n\n")
  Encoding(markdown) <- object@encoding
  markdown <- .adjustEncoding(markdown, "UTF8")
  markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
  markdown <- gsub("\n - ", "\n", markdown)
  markdown
}

#' @importFrom markdown markdownToHTML
.markdown2tmpfile <- function(markdown){
  mdFilename <- tempfile(fileext=".md")
  htmlFile <- tempfile(fileext=".html")
  cat(markdown, file=mdFilename)
  markdown::markdownToHTML(file=mdFilename, output=htmlFile)
  htmlFile
}

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
#' @aliases html html-method html,partition-method html,partitionCluster-method
#' @rdname html
#' @exportMethod html
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

setMethod("html", "partitionCluster", function(object, meta=NULL, from=1, to=10, filename=NULL, type="debate"){
  for (i in from:to){
    html(object@partitions[[i]], meta=meta, filename=NULL, type="debate")
  }
})