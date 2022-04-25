#' Restore S4 textstat objects
#' 
#' Using the reference semantics of `data.table` objects (i.e. inplace
#' modification) has great advantages for  memory efficiency. But there may be
#' unexpected behavior when reloading an S4 `textstat` object (including classes
#' inheriting from `textstat`) with a `data.table` in the `stat` slot. The
#' `restore` method will copy the `data.table` once to have a restored object
#' that works after saving / reloading it.
#' @param x An S4 `textstat` object or *.rds file to restore.
#' @export restore
#' @rdname restore
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
#' attr(problemprone@@stat, ".internal.selfref")
#' identical(attr(problemprone@@stat, ".internal.selfref"), new("externalptr"))
#' problemprone@@stat[, "newcol" := TRUE]
#' "newcol" %in% colnames(problemprone@@stat) # is FALSE!
#' problemprone@@stat <- data.table::copy(problemprone@@stat)
#' problemprone@@stat[, "newcol" := TRUE]
#' "newcol" %in% colnames(problemprone@@stat) # is TRUE now
setGeneric("restore", function(x) standardGeneric("restore"))


#' @rdname restore
#' @examples
#' 
#' # Restore stored S4 object with data.table slot
#' k <- kwic("REUTERS", query = "oil")
#' kwicfile <- tempfile(fileext = ".rds")
#' saveRDS(k, file = kwicfile)
#' 
#' k2 <- restore(kwicfile)
#' k3 <- enrich(k2, s_attribute = "id")
setMethod("restore", "character", function(x){
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
})

#' @details It is not possible to add columns to the `data.table` in the `stat`
#'   slot of a `textclass` object, when the object has been saved and loaded
#'   using `save()`/`load()`. This scenario also applies, when you agree to save
#'   the objects of an interactive R session are stored and load objects when
#'   starting the next interactive R session. The `restore()` method will create
#'   a copy of the `data.table`, so that inplace modifications are possible.
#' @examples
#' 
#' # Restore already loaded object
#' k <- kwic("REUTERS", query = "oil")
#' rdata_file <- tempfile(fileext = ".RData")
#' save(k, file = rdata_file)
#' rm(k)
#' load(rdata_file)
#' restore(k) # now it is possible to columns by reference
#' k@@stat[, "newcol" := TRUE]
#' @rdname restore
setMethod("restore", "textstat", function(x){
  x@stat <- copy(x@stat)
  invisible(x)
})

