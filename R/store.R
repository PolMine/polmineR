#' Store objects as Excel-file.
#' 
#' @param .Object An object that can be processed.
#' @param filename Name of the file to write.
#' @param rows The rows of the table to export.
#' @param ... Further arguments.
#' @exportMethod store
#' @rdname store
#' @name store
setGeneric("store", function(.Object, ...) standardGeneric("store"))

#' @rdname store
setMethod("store", "textstat", function(.Object, filename = tempfile(fileext = ".xlsx"), rows = 1L:nrow(.Object)){
  if (!requireNamespace(package = "openxlsx", quietly = TRUE)){
    stop("Package 'openxlsx' required but not available")
  }
  openxlsx::write.xlsx(x = .Object@stat[rows], file = filename, asTable = TRUE)
  filename
})


#' @rdname store
setMethod("store", "data.frame", function(.Object, filename = tempfile(fileext = ".xlsx"), rows = 1L:nrow(.Object)){
  if (!requireNamespace(package = "openxlsx", quietly = TRUE)){
    stop("Package 'openxlsx' required but not available")
  }
  openxlsx::write.xlsx(x = .Object[rows,], file = filename, asTable = TRUE)
  filename
})


#' @rdname store
setMethod("store", "kwic", function(.Object, filename = tempfile(fileext = ".xlsx"), rows = 1L:nrow(.Object)){
  if (!requireNamespace(package = "openxlsx", quietly = TRUE)) stop("Package 'openxlsx' required but not available")
  openxlsx::write.xlsx(x = .Object@table, file = filename, asTable = TRUE)
  filename
})