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
#' @export as.data.table.textstat
as.data.table.textstat <- function(x) x@stat

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
  if (nrow(x@stat) == 0) warning("indexing is pointless because data.table is empty")
  if (is.null(key(x@stat))) setkeyv(x@stat, cols = x@p_attribute)
  if (missing(j)){
    x@stat <- x@stat[eval(i, envir = x@stat)]
    return(x)
  } else {
    return( x@stat[i,j, with = FALSE] )
  }
})

setAs(from = "textstat", to = "htmlwidget", def = function(from){
  DT::datatable(from@stat, options = list(pageLength = getOption("polmineR.pagelength"), lengthChange = FALSE))
})

setAs(from = "cooccurrences", to = "htmlwidget", def = function(from){
  dt <- copy(round(from)@stat)
  colnames(dt) <- gsub("count_", "n_", colnames(dt))
  DT::datatable(
    dt,
    options = list(pageLength = getOption("polmineR.pagelength"), lengthChange = FALSE),
    rownames = FALSE
  )
})

setAs(from = "features", to = "htmlwidget", def = function(from){
  dt <- copy(round(from)@stat)
  for (i in grep("_id", colnames(dt), value = TRUE)) dt[, eval(i) := NULL]
  colnames(dt) <- gsub("count_", "n_", colnames(dt))
  DT::datatable(dt, options = list(pageLength = getOption("polmineR.pagelength"), lengthChange = FALSE))
})

#' @importFrom knitr knit_print
#' @exportMethod knit_print
#' @rdname textstat-class
#' @param options Chunk options.   
setMethod("knit_print", "textstat", function(x, options = knitr::opts_chunk, ...){
  y <- as(x, "htmlwidget")
  knit_print(y, options = options)
})
