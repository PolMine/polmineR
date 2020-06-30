#' @include S4classes.R p_attributes.R
NULL


#' @exportMethod head
setMethod("head", "textstat", function(x, ...) head(x@stat, ...) )

#' @exportMethod tail
setMethod("tail", "textstat", function(x, ...) tail(x@stat, ...) )

#' @exportMethod dim
setMethod("dim", "textstat", function(x) dim(x@stat))

#' @exportMethod nrow
setMethod("nrow", "textstat", function(x) nrow(x@stat))

#' @exportMethod ncol
setMethod("ncol", "textstat", function(x) ncol(x@stat))


#' @param digits Number of digits.
#' @rdname textstat-class
#' @exportMethod round
#' @details The \code{round()}-method looks up all numeric columns in the
#'   \code{data.table} in the \code{stat}-slot of the \code{textstat} object and
#'   rounds values of these columns to the number of decimal places specified by
#'   argument \code{digits}.
setMethod("round", "textstat", function(x, digits = 2L){
  if (is(x@stat)[1] == "data.table"){
    if (nrow(x@stat) > 1L){
      column_classes <- sapply(x@stat, function(column) is(column)[1])
      numeric_columns <- which(column_classes == "numeric")
      for (i in numeric_columns) x@stat[, colnames(x@stat)[i] := round(x@stat[[i]], digits)]
    }
  }
  x
})

#' @exportMethod colnames
setMethod("colnames", "textstat", function(x) colnames(x@stat))

#' @exportMethod names
setMethod("names", "textstat", function(x) x@name)

#' @exportMethod sort
#' @rdname textstat-class
setMethod("sort", "textstat", function(x, by, decreasing = TRUE){
  setkeyv(x@stat, cols = by)
  setorderv(x@stat, cols = by, order = ifelse(decreasing == TRUE, -1L, 1L), na.last = TRUE)
  return(x)
})

#' @rdname textstat-class
#' @exportMethod as.bundle
setGeneric("as.bundle", function(object, ...) standardGeneric("as.bundle"))

setMethod("as.bundle", "textstat", function(object){
  new(
    paste(is(object)[1], "_bundle", sep = ""),
    objects = setNames(list(object), object@name),
    corpus = object@corpus,
    encoding = object@encoding
  )
})



#' @exportMethod +
#' @docType methods
#' @rdname textstat-class
setMethod("+", signature(e1 = "textstat", e2 = "textstat"), function(e1, e2){
  if (e1@corpus != e2@corpus) warning("Please be careful - partition is from a different CWB corpus")
  retval <- as.bundle(e1)
  retval@objects[[length(retval@objects) + 1L]] <- e2
  names(retval@objects)[length(retval@objects)] <- e2@name
  retval
})

#' @exportMethod subset
#' @rdname textstat-class
#' @examples
#' sc <- partition("GERMAPARLMINI", speaker = "Angela Dorothea Merkel")
#' cnt <- count(sc, p_attribute = c("word", "pos"))
#' cnt_min <- subset(cnt, pos %in% c("NN", "ADJA"))
#' cnt_min <- subset(cnt, pos == "NE")
#' @param subset A logical expression indicating elements or rows to keep.
setMethod("subset", "textstat", function(x, subset){
  expr <- substitute(subset)
  x@stat <- x@stat[eval(expr, envir = x@stat)]
  x
})


#' @rdname textstat-class
#' @export
#' @method as.data.table textstat
#' @examples 
#' 
#' # Get statistics in textstat object as data.table
#' count_dt <- corpus("REUTERS") %>%
#'   subset(grep("saudi-arabia", places)) %>% 
#'   count(p_attribute = "word") %>%
#'   as.data.table()
as.data.table.textstat <- function(x, ...){
  if (length(list(...)) > 0L){
    warning(
      "Further arguments passed into the as.data.table method for textstat class objects ",
      "or objects inheriting from the textstat class remain unused."
    )
  }
  x@stat
}

#' @exportMethod as.data.frame
setMethod("as.data.frame", "textstat", function(x) as.data.frame(x@stat) )

#' @rdname textstat-class
setMethod("show", "textstat", function(object) {
  if (Sys.getenv("RSTUDIO") == "1" && interactive() && is.na(Sys.getenv("NOT_CRAN", unset = NA))){
    view(object)
  } else {
    message(sprintf("Object of class '%s'", is(object)[1]))
  }
})



#' @exportMethod p_attributes
#' @param object a textstat object
#' @rdname textstat-class
setMethod("p_attributes", "textstat", function(.Object) .Object@p_attribute)

#' @exportMethod [[
setMethod("[[", "textstat", function(x, i){
  if (nrow(x@stat) == 0){
    warning("indexing is pointless because data.table is empty")
  }
  x@stat[[i]]
})

#' @exportMethod [
#' @importFrom data.table key
setMethod("[", "textstat", function(x, i, j){
  if (nrow(x@stat) == 0L) warning("Indexing is not possible because data.table is empty.")

  # Note that i cannot be a call/expression (such as word %in% c("price", "revenue"))
  # in the context of a S4 method

  if (is.character(i) && is.null(key(x@stat))){
    if (x@p_attribute %in% colnames(x@stat)) setkeyv(x@stat, cols = x@p_attribute)
  }

  if (missing(j)){
    x@stat <- x@stat[eval(i, envir = x@stat)]
    return(x)
  } else {
    return( x@stat[i,j, with = FALSE] )
  }
})


# setAs(from = "textstat", to = "htmlwidget", def = function(from){
#   DT::datatable(
#     from@stat,
#     options = list(
#       pageLength = getOption("polmineR.pagelength"),
#       lengthChange = FALSE)
#   )
# })

setAs(from = "data.table", to = "htmlwidget", def = function(from){
  DT::datatable(
    from,
    extensions = "Buttons",
    filter = "top",
    options = c(list(
      pageLength = getOption("polmineR.pagelength"),
      lengthMenu = c(10,25,50,100,250),
      lengthChange = TRUE
    ),
    if (getOption("polmineR.buttons")){
      list(
        dom = "<'row'<'col-md-3'l><'col-md-6'><'col-md-3'B>><'row'<'col-md-12't>><'row'<'col-md-6'i><'col-md-6'p>>",
        buttons = c('copy', 'excel', 'pdf')
      )
    } else NULL
    ),
    rownames = FALSE,
    selection = "single"
  )
})

setAs(from = "textstat", to = "htmlwidget", def = function(from){
  dt <- format(from)
  colnames(dt) <- gsub("count_", "n_", colnames(dt))
  as(dt, "htmlwidget")
})


# setAs(from = "features", to = "htmlwidget", def = function(from){
#   dt <- copy(round(from)@stat)
#   for (i in grep("_id", colnames(dt), value = TRUE)) dt[, eval(i) := NULL]
#   colnames(dt) <- gsub("count_", "n_", colnames(dt))
#   DT::datatable(dt, options = list(pageLength = getOption("polmineR.pagelength"), lengthChange = FALSE))
# })

#' @importFrom knitr knit_print
#' @exportMethod knit_print
#' @rdname textstat-class
#' @param options Chunk options.   
setMethod("knit_print", "textstat", function(x, options = knitr::opts_chunk, ...){
  y <- as(x, "htmlwidget")
  knit_print(y, options = options)
})
