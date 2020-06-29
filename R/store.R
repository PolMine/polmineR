#' Restore S4 object with data.table slots
#' 
#' Reloading an S4 object that has a slot with a \code{data.table} may result in
#' buggy behavior. this auxiliary function will copy the \code{data.table} once
#' to have a restored object that works.
#' 
#' @param filename A *.rds file to restore.
#' @rdname restore
#' @examples
#' k <- kwic("REUTERS", query = "oil")
#' kwicfile <- tempfile()
#' saveRDS(k, file = kwicfile)
#' k <- restore(filename = kwicfile)
#' k2 <- enrich(k, s_attribute = "id")
#' @export restore
restore <- function(filename){
  y <- readRDS(file = filename)
  if (isS4(y)){
    all_slots <- getSlots(class(y))
    dt_slots <- names(all_slots[which(all_slots == "data.table")])
    if (length(dt_slots) > 0L){
      for (slot in dt_slots) slot(object = y, name = slot) <- copy(slot(y, slot))
    }
  }
  y
}
