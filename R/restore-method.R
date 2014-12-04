#' @exportMethod restore
setGeneric("restore", function(object, ...) standardGeneric("restore"))

#' @rdname store
setMethod("restore", "session", function(object){
  files <- list.files(
    file.path(
      Sys.getenv("POLMINER_DIR"),
      get("session", ".GlobalEnv")@project
    ),
    full.names=TRUE, include.dirs=FALSE, recursive=TRUE
  )
  for (file in files){
    print(file)
    assign(
      x=print(gsub(".*?/(.*?)\\.RData", "\\1", file)),
      value=readRDS(file=file),
      envir=as.environment(".GlobalEnv")
    )
  }
})

#' @rdname store
setMethod("restore", "character", function(object){
  assign(
    "session",
    readRDS(file=file.path(Sys.getenv("POLMINER_DIR"), object, "session.RData")),
    envir=as.environment(".GlobalEnv")
  )
})


