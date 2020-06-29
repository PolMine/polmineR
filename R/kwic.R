#' @include S4classes.R
NULL


setAs(from = "kwic", to = "htmlwidget", def = function(from){
  dt <- format(from, align = FALSE)
  retval <- DT::datatable(
    dt,
    extensions = "Buttons",
    filter = "top",
    options = c(
      list(
        pageLength = getOption("polmineR.pagelength"), 
        lengthMenu = c(10,25,50,100,250,500),
        lengthChange = TRUE
      ), 
      if (getOption("polmineR.buttons")){
        list(
          dom = "<'row'<'col-md-3'l><'col-md-6'><'col-md-3'B>><'row'<'col-md-12't>><'row'<'col-md-6'i><'col-md-6'p>>",
          buttons = c('copy', 'excel', 'pdf')
        )
      } else {
        NULL
      }
    ),
    escape = FALSE,
    selection = "single",
    rownames = FALSE
  )
  if ("node" %in% colnames(dt)) retval <- DT::formatStyle(retval, "node", textAlign = "center")
  if ("left" %in% colnames(dt)) retval <- DT::formatStyle(retval, "left", textAlign = "right")
  retval$dependencies[[length(retval$dependencies) + 1L]] <- htmltools::htmlDependency(
    name = "tooltips", version = "0.0.0",
    src = system.file(package = "polmineR", "css"), stylesheet = "tooltips.css"
  )
  retval
}
)

#' @rdname kwic-class
#' @docType method
#' @importFrom DT datatable formatStyle
setMethod("show", "kwic", function(object){
  y <- as(object, "htmlwidget")
  if (interactive()){
    if (
      isFALSE(getOption("polmineR.warn.size")) && 
      (is.null(getOption("DT.warn.size")) || isFALSE(getOption("DT.warn.size")))
    ){
      restore_value <- getOption("DT.warn.size")
      options("DT.warn.size" = FALSE)
      on.exit(options("DT.warn.size" = restore_value))
    }
    show(y)
  } else {
    return(y)
  }
})


#' @details The \code{knit_print} will be called by knitr when processing code
#'   chunks in Rmarkdown documents to include a \code{htmlwidget} into the
#'   resulting html document. It may be necessary to explicitly state
#'   "render=knit_print" in the chunk options.
#' @importFrom knitr knit_print
#' @exportMethod knit_print
#' @rdname kwic-class
#' @param pagelength An \code{integer} value, the number of kwic lines displayed
#'   per page in the \code{datatables} htmlwidget that is returned when
#'   \code{knit_print} is called on a \code{kwic} object.
#' @param options Chunk options.   
setMethod("knit_print", "kwic", function(x, pagelength = getOption("polmineR.pagelength"), options = knitr::opts_chunk, ...){
  y <- as(x, "htmlwidget")
  knit_print(y, options = options)
})


#' @rdname kwic-class
#' @docType method
#' @exportMethod as.character
#' @param fmt A format string passed into \code{sprintf} to format the node of a
#'   KWIC display.
#' @details The \code{as.character}-method will return a list of
#'   \code{character} vectors, concatenating the columns "left", "node" and
#'   "right" of the \code{data.table} in the \code{stat}-slot of the input
#'   \code{kwic}-class object. Optionally, the node can be formatted using a
#'   format string that is passed into \code{sprintf}.
#' @examples 
#' # extract node and left and right context as character vectors
#' oil <- kwic("REUTERS", query = "oil")
#' as.character(oil, fmt = NULL)
#' as.character(oil) # node wrapped into <i> tag by default
#' as.character(oil, fmt = "<b>%s</b>")
#' 
setMethod("as.character", "kwic", function(x, fmt = "<i>%s</i>"){
  if (!is.null(fmt)) x@stat[, "node" := sprintf(fmt, x@stat[["node"]])]
  apply(x@stat, 1L, function(r) paste(r[["left"]], r[["node"]], r[["right"]], sep = " "))
})

#' @docType methods
#' @rdname kwic-class
setMethod("[", "kwic", function(x, i){
  ids <- x@stat[["match_id"]][i]
  x@stat <- x@stat[which(x@stat[["match_id"]] %in% ids)]
  x@cpos <- x@cpos[x@cpos[["match_id"]] %in% x@stat[["match_id"]]]
  x
})

#' @details The \code{subset}-method will apply \code{subset} to the table in
#'   the slot \code{stat}, e.g. for filtering query results based on metadata (i.e.
#'   s-attributes) that need to be present.
#' @rdname kwic-class
#' @examples 
#' # subsetting kwic objects
#' oil <- corpus("REUTERS") %>%
#'   kwic(query = "oil") %>%
#'   subset(grepl("prices", right))
#' saudi_arabia <- corpus("REUTERS") %>%
#'   kwic(query = "Arabia") %>%
#'   subset(grepl("Saudi", left))
#' int_spd <- corpus("GERMAPARLMINI") %>%
#'   kwic(query = "Integration") %>%
#'   enrich(s_attribute = "party") %>%
#'   subset(grepl("SPD", party))
#'
setMethod("subset", "kwic", function(x, ...) {
  x@stat <- subset(x@stat, ...)
  x@cpos <- x@cpos[x@cpos[["match_id"]] %in% x@stat[["match_id"]]]
  x
})

#' @rdname kwic-class
#' @examples
#' # turn kwic object into data.frame with html tags
#' int <- corpus("GERMAPARLMINI") %>%
#'   kwic(query = "Integration")
#' 
#' as.data.frame(int) # Without further metadata
#' 
#' enrich(int, s_attributes = c("date", "speaker", "party")) %>%
#'   as.data.frame()
#'   
setMethod("as.data.frame", "kwic", function(x){
  if (all(c("left", "node", "right") %in% colnames(x@stat))){
    df <- data.frame(
      left = x@stat[["left"]],
      node = x@stat[["node"]],
      right = x@stat[["right"]],
      stringsAsFactors = FALSE
    )
    if (length(x@metadata) > 0L){
      df <- data.frame(
        meta = do.call(paste, c(lapply(x@metadata, function(s_attr) x@stat[[s_attr]]), sep = "<br/>")),
        df,
        stringsAsFactors = FALSE
      )
    }
  } else {
    stop("as.data.frame,kwic-method not yet implemented for lineview")
  }
  df
})


#' @rdname kwic-class
setMethod("length", "kwic", function(x) nrow(x@stat) )

#' @rdname kwic-class
setMethod("sample", "kwic", function(x, size){
  hits_unique <- unique(x@cpos[["match_id"]])
  if (size > length(hits_unique)){
    warning("argument size exceeds number of hits, returning original object")
    return(x)
  }
  x@cpos <- x@cpos[which(x@cpos[["match_id"]] %in% sample(hits_unique, size = size))]
  x <- enrich(x, table = TRUE)
  x <- enrich(x, s_attributes = x@metadata)
  x
})


#' @include partition.R context.R
NULL 

#' Perform keyword-in-context (KWIC) analysis.
#' 
#' Get concordances for the matches for a query / perform keyword-in-context
#' (kwic) analysis.
#' 
#' The method works with a whole CWB corpus defined by a  character vector, and
#' can be applied on a \code{partition}- or a \code{context} object.
#' 
#' If a \code{positivelist} is supplied, only those concordances will be kept that
#' have one of the terms from the \code{positivelist} occurr in the context of
#' the query match. Use argument \code{regex} if the positivelist should be
#' interpreted as regular expressions. Tokens from the positivelist will be
#' highlighted in the output table.
#' 
#' If a \code{negativelist} is supplied, concordances are removed if any of the
#' tokens of the \code{negativelist} occurrs in the context of the query match.
#' 
#' @return If there are no matches, or if all (initial) matches are dropped due to the
#' application of a positivelist, a \code{NULL} is returned.
#' 
#' @param .Object A (length-one) \code{character} vector with the name of a CWB
#'   corpus, a \code{partition} or \code{context} object.
#' @param query A query, CQP-syntax can be used.
#' @param cqp Either a logical value (\code{TRUE} if \code{query} is a CQP
#'   query), or a function to check whether query is a CQP query or not
#'   (defaults to auxiliary function \code{is.query}).
#' @param check A \code{logical} value, whether to check validity of CQP query
#'   using \code{check_cqp_query}.
#' @param left Number of tokens to the left of query match.
#' @param right Number of tokens to the right of query match.
#' @param s_attributes Structural attributes (s-attributes) to include into
#'   output table as metainformation.
#' @param cpos Logical, if \code{TRUE}, a \code{data.table} with the corpus
#'   positions ("cpos") of the hits and their surrounding context will be
#'   assigned to the slot "cpos" of the \code{kwic}-object that is returned.
#'   Defaults to \code{TRUE}, as the availability of the cpos-\code{data.table}
#'   will often be a prerequisite for further operations on the \code{kwic}
#'   object. Omitting the table may however be useful to minimize memory
#'   consumption.
#' @param p_attribute The p-attribute, defaults to 'word'.
#' @param boundary If provided, a length-one character vector stating an
#'   s-attribute that will be used to check the boundaries of the text.
#' @param stoplist Terms or ids to prevent a concordance from occurring in
#'   results.
#' @param positivelist Terms or ids required for a concordance to occurr in
#'   results
#' @param regex Logical, whether \code{stoplist}/\code{positivelist} is
#'   interpreted as regular expression.
#' @param verbose A \code{logical} value, whether to print messages.
#' @param progress A \code{logical} value, whether to show progress bar.
#' @param ... Further arguments, used to ensure backwards compatibility. If
#'   \code{.Object} is a \code{remote_corpus} of \code{remote_partition} object,
#'   the three dots (\code{...}) are used to pass arguments. Hence, it is
#'   necessary to state the names of all arguments to be passed explicity.
#' @docType methods
#' @seealso The return value is a \code{\link{kwic-class}} object; the
#'   documentation for the class explains the standard generic methods
#'   applicable to \code{\link{kwic-class}} objects. It is possible to read the
#'   whole text where a query match occurs, see the \code{\link{read}}-method.
#'   To highlight terms in the context of a query match, see the
#'   \code{\link{highlight}}-method.
#' @references 
#' Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum, pp. 71-93 (ch. 4).
#'
#' Jockers, Matthew L. (2014): \emph{Text Analysis with R for Students of Literature}.
#' Cham et al: Springer, pp. 73-87 (chs. 8 & 9).
#' @examples
#' use("polmineR")
#' 
#' # basic usage
#' K <- kwic("GERMAPARLMINI", "Integration")
#' if (interactive()) show(K)
#' oil <- corpus("REUTERS") %>% kwic(query = "oil")
#' if (interactive()) show(oil)
#' oil <- corpus("REUTERS") %>%
#'   kwic(query = "oil") %>%
#'   highlight(yellow = "crude")
#' if (interactive()) show(oil)
#' 
#' # increase left and right context and display metadata
#' K <- kwic(
#'   "GERMAPARLMINI",
#'   "Integration", left = 20, right = 20,
#'   s_attributes = c("date", "speaker", "party")
#' )
#' if (interactive()) show(K)
#' 
#' # use CQP syntax for matching
#' K <- kwic(
#'   "GERMAPARLMINI",
#'   '"Integration" [] "(Menschen|Migrant.*|Personen)"', cqp = TRUE,
#'   left = 20, right = 20,
#'   s_attributes = c("date", "speaker", "party")
#' )
#' if (interactive()) show(K)
#' 
#' # check that boundary of region is not transgressed
#' K <- kwic(
#'   "GERMAPARLMINI",
#'   '"Sehr" "geehrte"', cqp = TRUE,
#'   left = 100, right = 100,
#'   boundary = "date"
#' )
#' if (interactive()) show(K)
#' 
#' # use positivelist and highlight matches in context
#' K <- kwic("GERMAPARLMINI", query = "Integration", positivelist = "[Ee]urop.*", regex = TRUE)
#' K <- highlight(K, yellow = "[Ee]urop.*", regex = TRUE)
#' 
#' @exportMethod kwic
setGeneric("kwic", function(.Object, ...) standardGeneric("kwic") )



#' @exportMethod kwic
#' @docType methods
#' @rdname kwic
setMethod("kwic", "context", function(.Object, s_attributes = getOption("polmineR.meta"), cpos = TRUE, verbose = FALSE, ...){
  
  if ("meta" %in% names(list(...))) s_attributes <- list(...)[["meta"]]
  
  DT <- copy(.Object@cpos) # do not accidentily modify things
  setorderv(DT, cols = c("match_id", "cpos"))
  p_attr_decoded <- cl_id2str(
    corpus = .Object@corpus, p_attribute = .Object@p_attribute[1],
    id = DT[[paste(.Object@p_attribute[1], "id", sep = "_")]],
    registry = registry()
  )
  DT[, .Object@p_attribute[1] := as.nativeEnc(p_attr_decoded, from = .Object@encoding), with = TRUE]
  DT[, "direction" := sign(DT[["position"]]), with = TRUE]
  
  if (is.null(s_attributes)) s_attributes <- character()
  y <- new(
    "kwic",
    corpus = .Object@corpus,
    left = as.integer(.Object@left),
    right = as.integer(.Object@right),
    p_attribute = .Object@p_attribute,
    metadata = if (length(s_attributes) == 0L) character() else s_attributes,
    encoding = .Object@encoding,
    cpos = DT,
    stat = data.table()
  )
  y <- enrich(y, table = TRUE, s_attributes = s_attributes)
  if (isFALSE(cpos)) y@cpos <- data.table()
  y
})


#' @rdname kwic
#' @exportMethod kwic
setMethod("kwic", "slice", function(
  .Object, query, cqp = is.cqp,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  s_attributes = getOption("polmineR.meta"),
  p_attribute = "word", boundary = NULL, cpos = TRUE,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  verbose = TRUE, ...
){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) boundary <- list(...)[["sAttribute"]]
  if ("sAttribute" %in% names(list(...))) boundary <- list(...)[["s_attribute"]]
  if ("meta" %in% names(list(...))) s_attributes <- list(...)[["meta"]]
  
  # the actual work is done by the kwic,context-method
  # this method prepares a context-object and applies the
  # kwic method to that object
  ctxt <- context(
    .Object = .Object, query = query, cqp = cqp,
    p_attribute = p_attribute, boundary = boundary,
    left = left, right = right,
    stoplist = stoplist, positivelist = positivelist, regex = regex,
    count = FALSE, verbose = verbose
  )
  if (is.null(ctxt)){
    message("... no matches for query (or no matches left after applying stoplist/positivelist)")
    return(invisible(NULL))
  }
  retval <- kwic(.Object = ctxt, s_attributes = s_attributes, cpos = cpos)
  # if (!is.null(positivelist)) retval <- highlight(retval, highlight = list(yellow = positivelist), regex = regex)

  retval
})


#' @rdname kwic
#' @exportMethod kwic
setMethod("kwic", "partition", function(
  .Object, query, cqp = is.cqp,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  s_attributes = getOption("polmineR.meta"),
  p_attribute = "word", boundary = NULL, cpos = TRUE,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  verbose = TRUE, ...
){
  callNextMethod() 
})

#' @rdname kwic
#' @exportMethod kwic
setMethod("kwic", "subcorpus", function(
  .Object, query, cqp = is.cqp,
  left = getOption("polmineR.left"),
  right = getOption("polmineR.right"),
  s_attributes = getOption("polmineR.meta"),
  p_attribute = "word", boundary = NULL, cpos = TRUE,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  verbose = TRUE, ...
){
  callNextMethod() 
})




#' @rdname kwic
setMethod("kwic", "corpus", function(
  .Object, query, cqp = is.cqp, check = TRUE,
  left = as.integer(getOption("polmineR.left")),
  right = as.integer(getOption("polmineR.right")),
  s_attributes = getOption("polmineR.meta"),
  p_attribute = "word", boundary = NULL, cpos = TRUE,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  verbose = TRUE, progress = TRUE, ...
){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) boundary <- list(...)[["sAttribute"]]
  if ("s_attribute" %in% names(list(...))) boundary <- list(...)[["s_attribute"]]
  if ("meta" %in% names(list(...))) s_attributes <- list(...)[["meta"]]
  
  hits <- cpos(.Object, query = query, cqp = cqp, check = check, p_attribute = p_attribute, verbose = FALSE)
  if (is.null(hits)){
    message("No hits for query: ", query);
    return(invisible(NULL))
  }
  
  ctxt <- context(hits, left = left, right = right, corpus = .Object@corpus)

  ctxt@cpos[, paste(p_attribute, "id", sep = "_") := cl_cpos2id(corpus = .Object@corpus, p_attribute = p_attribute, cpos = ctxt@cpos[["cpos"]], registry = registry()), with = TRUE]
  
  ctxt@count <- nrow(hits)
  ctxt@boundary <- if (!is.null(boundary)) boundary else character()
  ctxt@p_attribute <- p_attribute
  ctxt@encoding <- .Object@encoding
  ctxt@partition <- new("partition", stat = data.table())

  # check that windows do not transgress s-attribute
  if (!is.null(boundary)){
    stopifnot(boundary %in% registry_get_s_attributes(ctxt@corpus))
    .message("checking that context positions to not transgress regions", verbose = verbose)
    ctxt <- enrich(ctxt, s_attribute = boundary, verbose = verbose, progress = progress)
    ctxt <- trim(ctxt, s_attribute = boundary, verbose = verbose, progress = progress)
  }
  
  # generate positivelist/stoplist with ids and apply it
  if (!is.null(positivelist)) ctxt <- trim(ctxt, positivelist = positivelist, regex = regex, verbose = verbose)
  if (is.null(ctxt)) return(NULL)
  if (!is.null(stoplist)) ctxt <- trim(ctxt, stoplist = stoplist, regex = regex, verbose = verbose)
  if (is.null(ctxt)) return(NULL)
  
  kwic(.Object = ctxt, s_attributes = s_attributes, cpos = cpos)
})




#' @rdname kwic
setMethod("kwic", "character", function(
  .Object, query, cqp = is.cqp, check = TRUE,
  left = as.integer(getOption("polmineR.left")),
  right = as.integer(getOption("polmineR.right")),
  s_attributes = getOption("polmineR.meta"),
  p_attribute = "word", boundary = NULL, cpos = TRUE,
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  verbose = TRUE, progress = TRUE, ...
){
  kwic(
    .Object = corpus(.Object),
    query = query,
    cqp = cqp,
    check = check,
    left = left,
    right = right,
    s_attributes = s_attributes,
    p_attribute = p_attribute,
    boundary = boundary,
    cpos = cpos,
    stoplist = stoplist,
    positivelist = positivelist,
    regex = regex,
    verbose = verbose,
    progress = progress,
    ...
  )
})

#' @rdname kwic
setMethod("kwic", "remote_corpus", function(.Object, ...){
  ocpu_exec(fn = "kwic", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, do.call = FALSE, .Object = as(.Object, "corpus"), ...)
})

#' @rdname kwic
setMethod("kwic", "remote_partition", function(.Object, ...){
  ocpu_exec(fn = "kwic", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "partition"), ...)
})

#' @rdname kwic
setMethod("kwic", "remote_subcorpus", function(.Object, ...){
  ocpu_exec(fn = "kwic", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "subcorpus"), ...)
})


#' @rdname kwic-class
#' @examples
#' # merge bundle of kwic objects into one kwic
#' reuters <- corpus("REUTERS")
#' queries <- c('"Saudi" "Arabia"', "oil", '"barrel.*"')
#' comb <- lapply(queries, function(qu) kwic(reuters, query = qu)) %>%
#'   as.bundle() %>%
#'   merge()
#'  
setMethod("merge", "kwic_bundle", function(x){
  
  table_list <- lapply(x@objects, function(obj) copy(obj@stat))
  cpos_list <- lapply(x@objects, function(obj) copy(obj@cpos))
  
  starting <- cumsum(sapply(table_list, function(tab) max(tab[["match_id"]])))
  starting <- c(0L, starting[-length(table_list)])
  
  lapply(
    seq_along(table_list),
    function(i) table_list[[i]][, "match_id" := table_list[[i]][["match_id"]] + starting[i]]
  )
  lapply(
    seq_along(cpos_list),
    function(i) cpos_list[[i]][, "match_id" := cpos_list[[i]][["match_id"]] + starting[i]]
  )

  new(
    "kwic",
    corpus = x@corpus,
    encoding = x@encoding,
    cpos = rbindlist(cpos_list),
    stat = rbindlist(table_list),
    p_attribute = unique(sapply(x@objects, function(obj) obj@p_attribute)),
    metadata = character(),
    left = unique(sapply(x@objects, function(obj) obj@left)),
    right = unique(sapply(x@objects, function(obj) obj@right)),

    name = character(),
    annotation_cols = character()
  )
})


#' @details Applying the \code{kwic}-method on a \code{partition_bundle} or
#'   \code{subcorpus_bundle} will return a single \code{kwic} object that
#'   includes a column 'subcorpus_name' with the name of the \code{subcorpus}
#'   (or \code{partition}) in the input object where the match for a concordance
#'   occurs.
#' @examples
#' # Apply kwic on partition_bundle/subcorpus_bundle
#' gparl_2009_11_10_speeches <- corpus("GERMAPARLMINI") %>%
#'   subset(date == "2009-11-10") %>%
#'   as.speeches(s_attribute_name = "speaker", progress = FALSE, verbose = FALSE)
#' k <- kwic(gparl_2009_11_10_speeches, query = "Integration")
#' @rdname kwic
setMethod("kwic", "partition_bundle", function(.Object, ..., verbose = FALSE){
  strucs_combined <- unlist(lapply(.Object@objects, slot, "strucs"))
  strucs_obj_name <- unname(unlist(lapply(.Object@objects, function(obj) rep(obj@name, times = length(obj@strucs)))))
  
  if (verbose) message("... merging subcorpora/partitions into one single subcorpus")
  sc <- merge(.Object)
  
  k <- kwic(sc, ..., verbose = FALSE)
  cols_old <- copy(colnames(k))
  k_cpos <- k@cpos[k@cpos[["position"]] == 0][, {.SD[.SD[["cpos"]] == min(.SD[["cpos"]])]}, by = "match_id"]
  k_cpos[, c("match_id", "cpos")][k@stat, on = "match_id"]
  match_strucs <- cl_cpos2struc(
    corpus = get_corpus(sc),
    s_attribute = sc@s_attribute_strucs,
    cpos = k_cpos[["cpos"]],
    registry = registry()
  )
  k@stat[, "subcorpus_name" := strucs_obj_name[match(match_strucs, unname(strucs_combined))]]
  setcolorder(k@stat, neworder = c("subcorpus_name", cols_old))
  k
})

#' @rdname kwic
setMethod("kwic", "subcorpus_bundle", function(.Object, ...) callNextMethod())