#' @include session_class.R
NULL

#' store objects
#' 
#' store and restore
#' 
#' @param object an object
#' @rdname store
#' @name store
#' @aliases restore
NULL


#' @exportMethod store
setGeneric("store", function(object, ...) standardGeneric("store"))

#' @rdname store
setMethod("store", "textstat", function(object){
  whereToStore <- file.path(
    Sys.getenv("POLMINER_DIR"),
    get("session", ".GlobalEnv")@project,
    class(object),
    paste(
      strsplit(deparse(sys.call(-1)), "\\(|\\)|,")[[1]][2],
      ".RData", sep=""
    )
  )
  saveRDS(object=object, file=whereToStore)
  return(whereToStore)
})

#' @rdname store
setMethod("store", "session", function(object){
  saveRDS(
    object=object,
    file.path(
      Sys.getenv("POLMINER_DIR"),
      object@project,
      "session.RData"
    )
  )
})

#' @rdname as.mallet-method
#' @importClassesFrom rJava jobjRef
setMethod("store", "jobjRef", function(object, filename=NULL){
  if (require("rJava", quietly=TRUE)){
    message("... rJava-package loaded")
  } else {
    warning("rJava package not available")
    stop()
  }
  if (is.null(filename)) filename <- tempfile()
  fileOutputStream <- new(rJava::J("java/io/FileOutputStream"), filename)
  objectStream <- new(rJava::J("java/io/ObjectOutputStream"), fileOutputStream)
  objectStream$writeObject(object)
  objectStream$close()
  # somewhat different alternative
  # fos <- .jnew("java/io/FileOutputStream", filename)
  # oos <- .jnew("java/io/ObjectOutputStream", .jcast(fos, "java/io/OutputStream"))
  # oos$writeObject(object)
  # oos$close()
  filename
})






