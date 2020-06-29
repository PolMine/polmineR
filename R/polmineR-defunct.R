#' Defunct functionality
#' 
#' @aliases polmineR-defunct
#' @param ... Any arguments that have been used by defunct functions/methods.
#' @name polmineR-defunct
#' @rdname polmineR-defunct
NULL

#' @export
#' @rdname polmineR-defunct
store <- function(...){
  .Defunct(
    msg = paste(
      "The 'store()' method is defunct and has been removed from the polmineR package.",
      "Recommended workflow: Use the 'format(x)' method to derive a formatted 'data.table' from object and ",
      "save it using 'openxlsx::write.xlsx()'.",
      collapse = " "
    )
  )
}

#' @export
#' @rdname polmineR-defunct
mail <- function(...){
  .Defunct(
    msg = paste(
      "The 'mail()' method is defunct and has been removed from the polmineR package.",
      "Use the buttons designed for exporting data of the DataTables tables instead.",
      collapse = " "
    )
  )
}