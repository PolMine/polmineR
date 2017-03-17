#' Display and read full text
#' 
#' Generate text (i.e. html) and read it in the viewer pane of RStudio. If called on
#' a \code{"partitionBundle"}-object, skip through the partitions contained in the
#' bundle.
#' 
#' @param .Object an object to be read (\code{"partition" or "partitionBundle"})
#' @param meta a character vector supplying s-attributes for the metainformation
#'   to be printed, if not stated explicitly, session settings will be used
#' @param highlight a list
#' @param tooltips a list
#' @param verbose logical
#' @param cpos logical
#' @param col column
#' @param partitionBundle a partitionBundle object
#' @param def ...
#' @param i ...
#' @param type ...
#' @param cqp a list of character vectors with regular expressions to
#'   highlight relevant terms or expressions; the names of the list provide the
#'   colors
#' @param cutoff maximum number of tokens to display
#' @param ... further parameters passed into read
#' @exportMethod read
#' @rdname read-method
#' @examples
#' \donttest{
#' use("polmineR.sampleCorpus")
#' options("polmineR.meta" = "text_date")
#' merkel <- partition(
#'   "PLPRBTTXT",
#'    text_date="2009-11-10", text_name="Angela Dorothea Merkel",
#'    type="plpr"
#'  )
#'  read(merkel, meta=c("text_name", "text_date"))
#'  read(
#'    merkel,
#'    highlight = list(yellow=c("Deutschland", "Bundesrepublik"), lightgreen="Regierung"),
#'    meta = c("text_name", "text_date")
#' )
#' all <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE, type="plpr")
#'    
#' speeches <- as.speeches(all, sAttributeDates="text_date", sAttributeNames="text_name", gap=500)
#' read(speeches)
#' migVocab <- count(speeches, query=c("Migration", "Integration", "Zuwanderung"))
#' read(migVocab, col="Integration", partitionBundle=speeches)
#' }
#' @seealso For concordances / a keword-in-context display, see \code{\link{kwic}}.
setGeneric("read", function(.Object, ...) standardGeneric("read"))

#' @rdname read-method
setMethod(
  "read", "partition",
  function(
    .Object, meta = NULL,
    highlight = list(), cqp = FALSE, tooltips = NULL,
    verbose = TRUE, cpos = FALSE, cutoff = getOption("polmineR.cutoff"), ...
    ){
  if (is.null(meta)) meta <- getOption("polmineR.meta")
  stopifnot(all(meta %in% sAttributes(.Object@corpus)))
  if (any(cqp) == TRUE) cpos <- TRUE
  fulltextHtml <- html(
    .Object, meta = meta, highlight = highlight, cqp = cqp, cpos = cpos, tooltips = tooltips, cutoff = cutoff, ...)
  if(require("htmltools", quietly = TRUE)){
    htmltools::html_print(fulltextHtml)  
  } else {
    warning("package htmltools required, but not available")
  }
})

#' @rdname read-method
setMethod("read", "partitionBundle", function(.Object, highlight = list(), cqp = FALSE, cpos = FALSE, ...){
  for (i in c(1:length(.Object@objects))){
    read(.Object@objects[[i]], highlight=highlight, cqp=cqp, cpos=cpos, ...)
    key <- readline("Enter 'q' to quit, any other key to continue. ")
    if (key == "q") break
  }
})

#' @rdname read-method
setMethod("read", "data.table", function(.Object, col, partitionBundle, cqp = FALSE, highlight=list(), cpos=FALSE, ...){
  stopifnot(col %in% colnames(.Object))
  DT <- .Object[which(.Object[[col]] > 0)]
  partitionsToGet <- DT[["partition"]]
  if (col == "TOTAL") col <- colnames(.Object)[2:(ncol(.Object)-1)]
  toRead <- as.bundle(lapply(partitionsToGet, function(x) partitionBundle@objects[[x]]))
  read(toRead, highlight=list(yellow=col), cqp=cqp, ...)
})

#' @rdname read-method
setMethod("read", "hits", function(.Object, def, i=NULL, ...){
  if (is.null(i)){
    for (i in c(1:nrow(.Object@dt))){
      sAttrs <- lapply(setNames(def, def), function(x) .Object@dt[[x]][i])
      read(partition(.Object@corpus, def=sAttrs, ...))
      readline(">> ")
    }
  }
})

#' @rdname read-method
setMethod("read", "kwic", function(.Object, i, type=NULL){
  fulltext <- html(.Object, i=i, type=type)
  htmltools::html_print(fulltext)
})

#' @rdname read-method
setMethod("read", "Regions", function(.Object, meta = NULL){
  toShow <- getTokenStream(.Object)
  if (is.null(meta) == FALSE){
    .getMetadata <- function(.BY, meta){
      lapply(
        setNames(meta, meta),
        function(M){
          as.utf8(
            CQI$struc2str(
              .Object@corpus,
              M,
              CQI$cpos2struc(.Object@corpus, M, .BY[[1]])
            ))
        }
      )
    }
    toShow <- toShow[, .getMetadata(.BY, meta), by = c("cpos_left", "cpos_right", "text"), with = TRUE]
  }
  toShow[["cpos_left"]] <- NULL
  toShow[["cpos_right"]] <- NULL
  setcolorder(toShow, c(meta, "text"))
  show(DT::datatable(toShow))
})