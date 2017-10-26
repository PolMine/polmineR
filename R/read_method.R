#' @include regions_class.R
NULL

#' Display full text.
#' 
#' Generate text (i.e. html) and read it in the viewer pane of RStudio. If called on
#' a \code{"partitionBundle"}-object, skip through the partitions contained in the
#' bundle.
#' 
#' To prepare the html output, the method \code{read} will call \code{html} and
#' \code{as.markdown} subsequently, the latter method being the actual worker. Consult
#' these methods to understand how preparing the output works.
#' 
#' The param \code{highlight} can be used to highlight terms. It is expected to be a
#' named list of character vectors, the names providing the colors, and the vectors
#' the terms to be highlighted.
#' 
#' @param .Object an object to be read (\code{"partition" or "partitionBundle"})
#' @param meta a character vector supplying s-attributes for the metainformation
#'   to be printed; if not stated explicitly, session settings will be used
#' @param template template to format output
#' @param highlight a named list of character vectors (see details)
#' @param tooltips a named list (names are colors, vectors are tooltips)
#' @param verbose logical
#' @param cpos logical, if TRUE, corpus positions will be assigned (invisibly) to a cpos
#' tag of a html element surrounding the tokens
#' @param col column of \code{data.table} with terms to be highlighted
#' @param partitionBundle a partitionBundle object
#' @param def a named list used to define a partition (names are s-attributes, vectors are
#' values of s-attributes)
#' @param i if \code{.Object} is an object of the classes \code{kwic} or \code{hits},
#' the ith kwic line or hit to derive a partition to be inspected from
#' @param type the partition type, see documentation for \code{partition}-method
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
#' template <- jsonlite::fromJSON(system.file(package = "polmineR", "templates", "plpr.template.json"))
#' options(polmineR.templates = list("PLPRBTTXT" = template))
#' merkel <- partition(
#'   "PLPRBTTXT",
#'    text_date = "2009-11-10", text_name = "Angela Dorothea Merkel",
#'    type = "plpr"
#'  )
#'  read(merkel, meta = c("text_name", "text_date"))
#'  read(
#'    merkel,
#'    highlight = list(yellow = c("Deutschland", "Bundesrepublik"), lightgreen = "Regierung"),
#'    meta = c("text_name", "text_date")
#' )
#' 
#' all <- partition("PLPRBTTXT", list(text_id = ".*"), regex = TRUE, type = "plpr")
#' speeches <- as.speeches(
#'   all, sAttributeDates = "text_date", sAttributeNames = "text_name", gap = 500
#' )
#' read(speeches, meta = c("text_date", "text_name"))
#' migVocab <- count(speeches, query=c("Migration", "Integration", "Zuwanderung"))
#' read(migVocab, col = "Integration", partitionBundle = speeches)
#' }
#' @seealso For concordances / a keword-in-context display, see \code{\link{kwic}}.
setGeneric("read", function(.Object, ...) standardGeneric("read"))

#' @rdname read-method
setMethod(
  "read", "partition",
  function(
    .Object, meta = NULL,
    highlight = list(), cqp = FALSE, tooltips = NULL,
    verbose = TRUE, cpos = FALSE, cutoff = getOption("polmineR.cutoff"), 
    template = getTemplate(.Object),
    ...
  ){
    if (is.null(meta)){
      templateMeta <- getOption("polmineR.templates")[[.Object@corpus]][["metadata"]]
      meta <- if (is.null(templateMeta)) names(.Object@sAttributes) else templateMeta
    }
    stopifnot(all(meta %in% sAttributes(.Object@corpus)))
    if (any(cqp)) cpos <- TRUE
    fulltextHtml <- html(
      .Object, meta = meta, highlight = highlight, cqp = cqp,
      cpos = cpos, tooltips = tooltips, cutoff = cutoff, 
      template = template,
      ...
    )
    if (require("htmltools", quietly = TRUE)){
      htmltools::html_print(fulltextHtml)  
    } else {
      warning("package htmltools required, but not available")
    }
  })

#' @rdname read-method
setMethod("read", "partitionBundle", function(.Object, highlight = list(), cqp = FALSE, cpos = FALSE, ...){
  for (i in 1:length(.Object@objects)){
    read(.Object@objects[[i]], highlight = highlight, cqp = cqp, cpos = cpos, ...)
    key <- readline("Enter 'q' to quit, any other key to continue. ")
    if (key == "q") break
  }
})

#' @rdname read-method
setMethod("read", "data.table", function(.Object, col, partitionBundle, cqp = FALSE, highlight = list(), cpos = FALSE, ...){
  stopifnot(col %in% colnames(.Object))
  DT <- .Object[which(.Object[[col]] > 0)]
  partitionsToGet <- DT[["partition"]]
  if (col == "TOTAL") col <- colnames(.Object)[2:(ncol(.Object)-1)]
  toRead <- as.bundle(lapply(partitionsToGet, function(x) partitionBundle@objects[[x]]))
  read(toRead, highlight = list(yellow = col), cqp=cqp, ...)
})

#' @rdname read-method
setMethod("read", "hits", function(.Object, def, i = NULL, ...){
  if (is.null(i)){
    for (i in 1:nrow(.Object@dt)){
      sAttrs <- lapply(setNames(def, def), function(x) .Object@dt[[x]][i])
      read(partition(.Object@corpus, def = sAttrs, ...))
      readline(">> ")
    }
  }
})

#' @rdname read-method
setMethod("read", "kwic", function(.Object, i, type = NULL){
  fulltext <- html(.Object, i = i, type = type)
  htmltools::html_print(fulltext)
})

#' @rdname read-method
setMethod("read", "regions", function(.Object, meta = NULL){
  toShow <- getTokenStream(.Object)
  if (is.null(meta) == FALSE){
    .getMetadata <- function(.BY, meta){
      lapply(
        setNames(meta, meta),
        function(M){
          as.nativeEnc(
            CQI$struc2str(
              .Object@corpus,
              M,
              CQI$cpos2struc(.Object@corpus, M, .BY[[1]])
            ), from = .Object@encoding
            )
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