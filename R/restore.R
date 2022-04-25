#' Restore S4 textstat objects
#' 
#' Using the reference semantics of `data.table` objects (i.e. inplace
#' modification) has great advantages for  memory efficiency. But there may be
#' unexpected behavior when reloading an S4 `textstat` object (including classes
#' inheriting from `textstat`) with a `data.table` in the `stat` slot. Use
#' `restore` to copy the `data.table` once to have a restored object that works
#' for inplace operations after saving / reloading it.
#' @param x An rds file to restore (filename).
#' @export restore
#' @rdname restore
#' @seealso The \code{\link{cp}} function addresses a related scenario.
#' @examples
#' # Before moving to examples, this is a brief technical dip into the problem
#' # solved by restore(): If we load the rds file the default way with
#' # readRDS(), the data.table in the slot 'stat' will have the pointer '0x0'
#' # and the data.table cannot be augmented without having been copied
#' # previously.
#' 
#' k <- kwic("REUTERS", query = "oil")
#' kwicfile <- tempfile(fileext = ".rds")
#' saveRDS(k, file = kwicfile)
#' problemprone <- readRDS(file = kwicfile)
#' problemprone@@stat[, "newcol" := TRUE]
#' "newcol" %in% colnames(problemprone@@stat) # is FALSE!
#' 
#' attr(problemprone@@stat, ".internal.selfref")
#' identical(attr(problemprone@@stat, ".internal.selfref"), new("externalptr"))
#' 
#' # Restore stored S4 object with copy of data.table in 'stat' slot
#' k <- kwic("REUTERS", query = "oil")
#' kwicfile <- tempfile(fileext = ".rds")
#' saveRDS(k, file = kwicfile)
#' 
#' k2 <- restore(kwicfile)
#' enrich(k2, s_attribute = "id")
#' "id" %in% colnames(k2) # is TRUE
restore <- function(x){
  if (!file.exists(x)) stop("file does not exist")
  y <- readRDS(file = x)
  if (isS4(y)){
    all_slots <- getSlots(class(y))
    dt_slots <- names(all_slots[which(all_slots == "data.table")])
    if (length(dt_slots) > 0L){
      for (slot in dt_slots)
        slot(object = y, name = slot) <- copy(slot(y, slot))
    }
  }
  y
}

#' Copy S4 textstat object.
#' 
#' It is not possible to add columns to the `data.table` in the `stat` slot of a
#' `textclass` object, when the object has been saved and loaded using
#' `save()`/`load()`. This scenario applies for instance, when the objects of an
#' interactive R session are saved, and loaded when starting the next
#' interactive R session. The `cp()` function will create a copy of the object,
#' including an explicit copy of the `data.table` in the `stat` slot. Inplace
#' modifications of the new object are possible. The function can also be used
#' to avoid unwanted side effects of modifying an object.
#' @examples
#' k <- kwic("REUTERS", query = "oil")
#' rdata_file <- tempfile(fileext = ".RData")
#' save(k, file = rdata_file)
#' rm(k)
#' 
#' load(rdata_file)
#' k <- cp(k) # now it is possible to columns by reference
#' enrich(k, s_attribute = "id")
#' "id" %in% colnames(k)
#' @param x An object (`textstat` or class inheriting from `textstat`).
#' @rdname cp
#' @export
#' @seealso The \code{\link{restore}} function addesses a related scenario.
cp <- function(x){
  y <- x
  y@stat <- copy(x@stat)
  y
}

