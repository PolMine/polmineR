#' turn partition into markdown
#' 
#' for further processing to html
#' 
#' @param object object to be converted
#' @param ... further arguments
#' @rdname as.markdown
#' @exportMethod as.markdown
setGeneric("as.markdown", function(object, ...) {standardGeneric("as.markdown")})

#' @importFrom rcqp cqi_struc2str cqi_cpos2str
#' @rdname as.markdown
setMethod("as.markdown", "plprPartition", function(object, meta){
  # in the function call, meta is actually not needed, its required by the calling function
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
      meta <- polmineR:::.adjustEncoding(meta, "latin1")
    }
    plainText <- paste(
      cqi_cpos2str(paste(
        object@corpus, '.word', sep=''),
        c(object@cpos[i,1]:object@cpos[i, 2])),
      collapse=" "
    )
    Encoding(plainText) <- object@encoding
    if (type[i] == "interjection") plainText <- paste("\n> ", plainText, "\n", sep="")
    retval <- paste(meta, plainText)
    return(retval)
  })
  # Encoding(markdown) <- object@encoding
  markdown <- paste(markdown, collapse="\n\n")
  markdown <- polmineR:::.adjustEncoding(markdown, "UTF8")
  markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", markdown)
  markdown <- gsub("\n - ", "\n", markdown)
  markdown
})

#' @importFrom rcqp cqi_struc2str cqi_cpos2str cqi_cpos2struc
#' @rdname as.markdown
setMethod("as.markdown", "pressPartition", function(object, meta=c("text_newspaper", "text_date")){
  articles <- apply(object@cpos, 1, function(row) {
    cpos <- c(row[1]:row[2])
    textStruc <- cqi_cpos2struc(paste(object@corpus, ".text", sep=""), row[1])
    pStrucs <- rcqp::cqi_cpos2struc(paste(object@corpus, ".p_type", sep=""), cpos)
    chunks <- split(cpos, pStrucs)
    pType <- rcqp::cqi_struc2str(paste(object@corpus, ".p_type", sep=""), as.numeric(names(chunks)))
    article <- Map(
      function(pType, chunk){
        meta <- 
          wordVector <- rcqp::cqi_cpos2str(paste(object@corpus, ".word", sep=""), chunk)
        para <- paste(wordVector, collapse=" ")
        Encoding(para) <- object@encoding
        para
      },
      pType, chunks
    )
    metaInformation <- sapply(
      meta,
      function(x) {
        retval <- cqi_struc2str(paste(object@corpus, ".", x, sep=""), textStruc)
        Encoding(retval) <- object@encoding
        retval
      }      )
    metaInformation <- paste(metaInformation, collapse=", ") # string will be converted to UTF-8
    article <- c(
      meta=metaInformation,
      article)
    .formattingFactory <- list(
      meta=function(x) paste("### ", x, sep=""),
      title=function(x) paste("## ", x, sep=""),
      teaser=function(x) paste("_", x, "_\n", sep=""),
      body=function(x) paste(x, "\n", sep="")  
    )
    formattedArticle <- lapply(
      c(1:length(article)),
      function(x) .formattingFactory[[names(article)[x]]](article[[x]])
    )
    articleMarkdown <- paste(formattedArticle, collapse="\n\n")
    # markdown <- polmineR:::.adjustEncoding(markdown, "UTF8")
    markdown <- gsub("(.)\\s([,.:!?])", "\\1\\2", articleMarkdown)
    # markdown <- gsub("\n - ", "\n", markdown)
    markdown
  })
  paste(c("\n", unlist(articles)), collapse='\n* * *\n')
})


