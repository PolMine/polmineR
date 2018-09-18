#' @include regions.R S4classes.R
NULL

#' Display full text.
#' 
#' Generate text (i.e. html) and display it in the viewer pane of RStudio for
#' reading it. If called on a \code{partition_bundle}-object, skip through the
#' partitions contained in the bundle.
#'
#' To prepare the html output, the method \code{read} will call \code{html} and
#' \code{as.markdown} subsequently, the latter method being the actual worker.
#' Consult these methods to understand how preparing the output works.
#'
#' The param \code{highlight} can be used to highlight terms. It is expected to
#' be a named list of character vectors, the names providing the colors, and the
#' vectors the terms to be highlighted. To add tooltips, use the param
#' \code{tooltips}.
#'
#' The method \code{read} is a high-level function that calls the methods
#' mentioned before. Results obtained through \code{read} can also be obtained
#' through combining these methods in a pipe using the package \code{magrittr}.
#' That may offer more flexibility, e.g. to highlight matches for CQP queries.
#' See examples and the documentation for the different methods to learn more.
#' 
#' @param .Object an object to be read (\code{"partition" or "partition_bundle"})
#' @param meta a character vector supplying s-attributes for the metainformation
#'   to be printed; if not stated explicitly, session settings will be used
#' @param template template to format output
#' @param highlight a named list of character vectors (see details)
#' @param tooltips a named list (names are colors, vectors are tooltips)
#' @param verbose logical
#' @param cpos logical, if TRUE, corpus positions will be assigned (invisibly) to a cpos
#' tag of a html element surrounding the tokens
#' @param col column of \code{data.table} with terms to be highlighted
#' @param partition_bundle a \code{partition_bundle} object
#' @param def a named list used to define a partition (names are s-attributes, vectors are
#' values of s-attributes)
#' @param i if \code{.Object} is an object of the classes \code{kwic} or \code{hits},
#' the ith kwic line or hit to derive a partition to be inspected from
#' @param type the partition type, see documentation for \code{partition}-method
#' @param cutoff maximum number of tokens to display
#' @param ... further parameters passed into read
#' @exportMethod read
#' @rdname read-method
#' @examples
#' use("polmineR")
#' merkel <- partition("GERMAPARLMINI", date = "2009-11-10", speaker = "Merkel", regex = TRUE)
#' read(merkel, meta = c("speaker", "date"))
#' read(
#'   merkel,
#'   highlight = list(yellow = c("Deutschland", "Bundesrepublik"), lightgreen = "Regierung"),
#'   meta = c("speaker", "date")
#' )
#' @seealso For concordances / a keword-in-context display, see \code{\link{kwic}}.
setGeneric("read", function(.Object, ...) standardGeneric("read"))

#' @rdname read-method
setMethod(
  "read", "partition",
  function(
    .Object, meta = NULL,
    highlight = list(), tooltips = list(),
    verbose = TRUE, cpos = TRUE, cutoff = getOption("polmineR.cutoff"), 
    template = get_template(.Object),
    ...
  ){
    if (is.null(meta)){
      templateMeta <- getOption("polmineR.templates")[[.Object@corpus]][["metadata"]]
      meta <- if (is.null(templateMeta)) names(.Object@s_attributes) else templateMeta
    }
    stopifnot(all(meta %in% s_attributes(.Object@corpus)))
    doc <- html(.Object, meta = meta,  cpos = cpos, cutoff = cutoff,  template = template, ...)
    
    if (length(highlight) > 0) {
      doc <- highlight(doc, highlight = highlight)
      if (length(tooltips) > 0){
        doc <- tooltips(doc, tooltips = tooltips)
      }
    }
    doc
  })

#' @rdname read-method
setMethod("read", "partition_bundle", function(.Object, highlight = list(), cpos = TRUE, ...){
  for (i in 1:length(.Object@objects)){
    read(.Object@objects[[i]], highlight = highlight, cpos = cpos, ...)
    key <- readline("Enter 'q' to quit, any other key to continue. ")
    if (key == "q") break
  }
})

#' @rdname read-method
setMethod("read", "data.table", function(.Object, col, partition_bundle, highlight = list(), cpos = FALSE, ...){
  stopifnot(col %in% colnames(.Object))
  DT <- .Object[which(.Object[[col]] > 0)]
  partitionsToGet <- DT[["partition"]]
  if (col == "TOTAL") col <- colnames(.Object)[2:(ncol(.Object)-1)]
  toRead <- as.bundle(lapply(partitionsToGet, function(x) partition_bundle@objects[[x]]))
  read(toRead, highlight = list(yellow = col), ...)
})

#' @rdname read-method
setMethod("read", "hits", function(.Object, def, i = NULL, ...){
  if (is.null(i)){
    for (i in 1L:nrow(.Object@stat)){
      sAttrs <- lapply(setNames(def, def), function(x) .Object@stat[[x]][i])
      read(partition(.Object@corpus, def = sAttrs, ...))
      readline(">> ")
    }
  }
})

#' @rdname read-method
setMethod("read", "kwic", function(.Object, i = NULL, type = registry_get_properties(corpus(.Object))["type"]){
  
  # if registry file does not have 'type' corpus property, a named NA vector arrives
  if (length(type) > 0L) if (is.na(type)) type <- NULL
  if (!is.null(type)) type <- unname(type)

  if (is.null(i)){
    for (i in 1L:length(.Object)){
      read(.Object, i = i, type = type)
      user <- readline(prompt = "Hit 'q' to quit or any other key to continue.\n")
      if (user == "q") return(invisible(NULL))
    }
  } else {
    fulltext <- html(.Object, i = i, type = type)
    if (interactive()) htmltools::html_print(fulltext)
  }
})

#' @rdname read-method
setMethod("read", "regions", function(.Object, meta = NULL){
  toShow <- get_token_stream(.Object)
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