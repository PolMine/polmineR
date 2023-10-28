#' @include regions.R S4classes.R
NULL

#' Display full text.
#' 
#' Generate text (i.e. html) and display it in the viewer pane of RStudio for
#' reading it. If called on a `partition_bundle`-object, skip through the
#' partitions contained in the bundle.
#'
#' To prepare the html output, the method `read()` will call `html()` and
#' `as.markdown()` subsequently, the latter method being the actual worker.
#' Consult these methods to understand how preparing the output works.
#'
#' The param `highlight()` can be used to highlight terms. It is expected to
#' be a named list of character vectors, the names providing the colors, and the
#' vectors the terms to be highlighted. To add tooltips, use the param
#' `tooltips`.
#'
#' The method `read()` is a high-level function that calls the methods
#' mentioned before. Results obtained through `read()` can also be obtained
#' through combining these methods in a pipe using the package 'magrittr'.
#' That may offer more flexibility, e.g. to highlight matches for CQP queries.
#' See examples and the documentation for the different methods to learn more.
#' 
#' @param .Object aAn object to be read (`partition` or `partition_bundle`).
#' @param meta a character vector supplying s-attributes for the metainformation
#'   to be printed; if not stated explicitly, session settings will be used
#' @param template template to format output
#' @param highlight a named list of character vectors (see details)
#' @param tooltips a named list (names are colors, vectors are tooltips)
#' @param annotation Object inheriting from `subcorpus` class. If provided,
#'   `highlight`, `tooltips` and `href` will be taken from the slot 'annotations'
#'   of this object.
#' @param verbose logical
#' @param cpos logical, if `TRUE`, corpus positions will be assigned (invisibly)
#'   to a cpos tag of a html element surrounding the tokens
#' @param col column of `data.table` with terms to be highlighted
#' @param partition_bundle A `partition_bundle` object.
#' @param def a named list used to define a partition (names are s-attributes,
#'   vectors are values of s-attributes)
#' @param i If `.Object` is an object of the classes `kwic` or `hits`, the ith
#'   kwic line or hit to derive a partition to be inspected from
#' @param type the partition type, see documentation for `partition()`-method
#' @param cutoff maximum number of tokens to display
#' @param ... Further parameters passed into `read()`.
#' @inheritParams href-function
#' @exportMethod read
#' @rdname read-method
#' @examples
#' use("polmineR")
#' merkel <- partition("GERMAPARLMINI", date = "2009-11-10", speaker = "Merkel", regex = TRUE)
#' if (interactive()) read(merkel, meta = c("speaker", "date"))
#' if (interactive()) read(
#'   merkel,
#'   highlight = list(yellow = c("Deutschland", "Bundesrepublik"), lightgreen = "Regierung"),
#'   meta = c("speaker", "date")
#' )
#' 
#' \dontrun{
#' pb <- as.speeches("GERMAPARLMINI", s_attribute_date = "date", s_attribute_name = "speaker")
#' pb <- pb[[ data.table::as.data.table(summary(pb))[size >= 500][["name"]] ]]
#' pb <- pb[[ 1:10 ]]
#' read(pb)
#' }
#' @seealso For concordances / a keword-in-context display, see \code{\link{kwic}}.
setGeneric("read", function(.Object, ...) standardGeneric("read"))

#' @rdname read-method
setMethod("read", "partition",
  function(
    .Object, meta = NULL,
    highlight = list(), tooltips = list(), href = list(),
    verbose = TRUE, cpos = TRUE, cutoff = getOption("polmineR.cutoff"), 
    template = get_template(.Object),
    ...
  ){
    newobj <- if (is.null(get_type(.Object))){
      "subcorpus"
    } else {
      switch(
        get_type(.Object),
        "plpr" = "plpr_subcorpus",
        "press" = "press_subcorpus"
      )
    }
    
    read(
      .Object = as(.Object, newobj),
      meta = meta,
      highlight = highlight, tooltips = tooltips, href = href,
      verbose = verbose, cpos = cpos, cutoff = cutoff,
      template = template,
      ...
    )
  }
)

#' @rdname read-method
setMethod(
  "read", "subcorpus",
  function(
    .Object, meta = NULL,
    highlight = list(), tooltips = list(), href = list(),
    annotation,
    verbose = TRUE, cpos = TRUE, cutoff = getOption("polmineR.cutoff"), 
    template = get_template(.Object),
    ...
  ){
    if (is.null(meta)){
      template_meta <- get_template(.Object)[["metadata"]]
      meta <- if (is.null(template_meta))
        names(.Object@s_attributes)
      else
        template_meta
    }
    stopifnot(all(meta %in% s_attributes(.Object@corpus)))
    doc <- html(
      .Object,
      meta = meta,
      cpos = cpos,
      cutoff = cutoff,
      template = template,
      ...
    )
    
    if (!missing(annotation)){
      if (!inherits(annotation, "subcorpus"))
        stop("argument 'annotation' required to inherit from subcorpus")
      
      if (!all(sapply(annotation@annotations, length) == nrow(annotation@cpos)))
        stop("length of all annotations not identical with number of regions")
      
      if ("highlight" %in% names(annotation@annotations)){
        highlight <- split(
          ranges_to_cpos(annotation@cpos),
          unlist(
            mapply(
              rep,
              x = annotation@annotations[["highlight"]],
              times = annotation@cpos[,2L] - annotation@cpos[,1L] + 1L,
              SIMPLIFY = FALSE
            )
          )
        )
      }
      
      if ("tooltips" %in% names(annotation@annotations)){
        tooltips <- as.list(
          setNames(
            unname(
              unlist(
                mapply(
                  rep,
                  x = annotation@annotations[["tooltips"]],
                  times = annotation@cpos[,2L] - annotation@cpos[,1L] + 1L,
                  SIMPLIFY = FALSE
                )
              )
            ),
            as.character(ranges_to_cpos(annotation@cpos))
          )
        )
      }
      
      if ("href" %in% names(annotation@annotations)){
        href <- as.list(
          setNames(
            unname(
              unlist(
                mapply(
                  rep,
                  x = annotation@annotations[["href"]],
                  times = annotation@cpos[,2L] - annotation@cpos[,1L] + 1L,
                  SIMPLIFY = FALSE
                )
              )
            ),
            as.character(ranges_to_cpos(annotation@cpos))
          )
        )
      }
    }
    
    if (length(highlight) > 0L)
      doc <- highlight(doc, highlight = highlight)
    
    if (length(tooltips) > 0L)
      doc <- tooltips(doc, tooltips = tooltips)
    
    if (length(href) > 0L)
      doc <- href(doc, href = href)

    doc
  }
)

#' @rdname read-method
setMethod("read", "partition_bundle", function(.Object, highlight = list(), cpos = TRUE, ...){
  for (i in 1L:length(.Object@objects)){
    y <- read(.Object@objects[[i]], highlight = highlight, cpos = cpos, ...)
    show(y)
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
  toRead <- as.bundle(
    lapply(
      partitionsToGet,
      function(x) partition_bundle@objects[[x]])
  )
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
setMethod("read", "kwic", function(.Object, i = NULL, type){
  
  if (missing(type)){
    properties <- corpus_properties(
      corpus = .Object@corpus,
      registry = .Object@registry_dir
    )
    if ("type" %in% properties){
      type <- corpus_property(
        corpus = .Object@corpus, registry = .Object@registry_dir,
        property = "type"
      )
    } else {
      type <- NULL
    }
      
  }
  if (length(type) > 0L) if (is.na(type)) type <- NULL
  if (!is.null(type)) type <- unname(type)
  
  if (is.null(i)) i <- seq_along(.Object)

  if (length(i) > 1L){
    for (j in i){
      read(.Object, i = j, type = type)
      user <- readline(
        prompt = "Hit 'q' to quit or any other key to continue.\n"
      )
      if (user == "q") return( invisible(NULL) )
    }
  } else {
    fulltext <- html(.Object, i = as.integer(i), type = type)
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
            cl_struc2str(
              corpus = .Object@corpus,
              s_attribute = M,
              struc = cl_cpos2struc(
                corpus = .Object@corpus,  registry = .Object@registry_dir,
                s_attribute = M, cpos = .BY[[1]]
              ),
              registry = .Object@registry_dir
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