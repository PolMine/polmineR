#' @include session-class.R store-method.R
NULL

##############################

#' @exportMethod corpus
#' @exportMethod corpus<-
NULL

setGeneric("corpus", function(object, ...) standardGeneric("corpus"))
setGeneric("corpus<-", function(object, value) standardGeneric("corpus<-"))

setMethod("corpus", "missing", function(object) rcqp::cqi_list_corpora())

#' @rdname session
setReplaceMethod("corpus", signature=c(object="session", value="character"), function(object, value) {
  object@corpus <- value
  object
})

#' @rdname session
setMethod("corpus", "session", function(object) object@corpus)

#' @rdname partition
setMethod("corpus", "partition", function(object) object@corpus)


##############################

#' @exportMethod project
#' @exportMethod project<-
NULL

setGeneric("project", function(object, ...) standardGeneric("project"))
setGeneric("project<-", function(object, value) standardGeneric("project<-"))

#' @rdname session
setReplaceMethod("project", signature=c(object="session", value="character"), function(object, value) {
  object@project <- value
  object
})

#' @rdname session
setMethod("project", "session", function(object) print(object@project))

#' @rdname session
setMethod("project", "missing", function(){
  list.dirs(Sys.getenv("POLMINER_DIR"), full.names=FALSE, recursive=FALSE)
})

######################

#' @exportMethod pAttribute
NULL




#' @rdname session
setMethod("pAttribute", "session", function(object) object@pAttribute)

#' @rdname session
setReplaceMethod("pAttribute", signature=c(object="session", value="character"), function(object, value) {
  object@pAttribute <- value
  object
})

##############################

#' @exportMethod leftContext
#' @exportMethod leftContext<-
NULL



setGeneric("leftContext", function(x) standardGeneric("leftContext"))
setGeneric("leftContext<-", function(x, value) standardGeneric("leftContext<-"))

#' @rdname session
setMethod("leftContext", "session", function(x) x@leftContext)

#' @rdname session
setReplaceMethod("leftContext", signature=c(x="session", value="numeric"), function(x, value) {
  x@leftContext <- value
  x
})

##############################

#' @exportMethod rightContext
#' @exportMethod rightContext<-
NULL



setGeneric("rightContext", function(x) standardGeneric("rightContext"))
setGeneric("rightContext<-", function(x, value) standardGeneric("rightContext<-"))

#' @rdname session
setMethod("rightContext", "session", function(x) x@rightContext)

#' @rdname session
setReplaceMethod("rightContext", signature=c(x="session", value="numeric"), function(x, value) {
  x@rightContext <- value
  x
})

##############################

#' @exportMethod minSignificance
#' @exportMethod minSignificance<-
NULL



setGeneric("minSignificance", function(x) standardGeneric("minSignificance"))
setGeneric("minSignificance<-", function(x, value) standardGeneric("minSignificance<-"))

#' @rdname session
setMethod("minSignificance", "session", function(x) x@minSignificance)

#' @rdname session
setReplaceMethod("minSignificance", signature=c(x="session", value="numeric"), function(x, value) {
  x@minSignificance <- value
  x
})


##############################

#' @exportMethod minFrequency
#' @exportMethod minFrequency<-
NULL



setGeneric("minFrequency", function(x) standardGeneric("minFrequency"))
setGeneric("minFrequency<-", function(x, value) standardGeneric("minFrequency<-"))

#' @rdname session
setMethod("minFrequency", "session", function(x) x@minFrequency)

#' @rdname session
setReplaceMethod("minFrequency", signature=c(x="session", value="numeric"), function(x, value) {
  x@minFrequency <- value
  x
})

##############################

#' @exportMethod posFilter
#' @exportMethod posFilter<-
NULL



setGeneric("posFilter", function(x) standardGeneric("posFilter"))
setGeneric("posFilter<-", function(x, value) standardGeneric("posFilter<-"))

#' @rdname session
setMethod("posFilter", "session", function(x) x@posFilter)

#' @rdname session
setReplaceMethod("posFilter", signature=c(x="session", value="character"), function(x, value) {
  x@posFilter <- value
  x
})

##############################

#' @exportMethod filterType
#' @exportMethod filterType<-
NULL


setGeneric("filterType", function(x) standardGeneric("filterType"))
setGeneric("filterType<-", function(x, value) standardGeneric("filterType<-"))

#' @rdname session
setMethod("filterType", "session", function(x) x@filterType)

#' @rdname session
setReplaceMethod("filterType", signature=c(x="session", value="character"), function(x, value) {
  x@filterType <- value
  x
})

##############################

#' @exportMethod kwicMetadata
#' @exportMethod kwicMetadata<-
NULL



setGeneric("kwicMetadata", function(x) standardGeneric("kwicMetadata"))
setGeneric("kwicMetadata<-", function(x, value) standardGeneric("kwicMetadata<-"))

#' @rdname session
setMethod("kwicMetadata", "session", function(x) x@kwicMetadata)

#' @rdname session
setReplaceMethod("kwicMetadata", signature=c(x="session", value="character"), function(x, value) {
  x@kwicMetadata <- value
  x
})


##############################

#' @exportMethod kwicNo
#' @exportMethod kwicNo<-
NULL


setGeneric("kwicNo", function(x) standardGeneric("kwicNo"))
setGeneric("kwicNo<-", function(x, value) standardGeneric("kwicNo<-"))

#' @rdname session
setMethod("kwicNo", "session", function(x) x@kwicNo)

#' @rdname session
setReplaceMethod("kwicNo", signature=c(x="session", value="numeric"), function(x, value) {
  x@kwicNo <- value
  x
})

##############################

#' @exportMethod metadata
#' @exportMethod metadata<-
NULL


setGeneric("metadata", function(x) standardGeneric("metadata"))
setGeneric("metadata<-", function(x, value) standardGeneric("metadata<-"))

#' @rdname session
setMethod("metadata", "session", function(x) x@metadata)

#' @rdname session
setReplaceMethod("metadata", signature=c(x="session", value="character"), function(x, value) {
  x@metadata <- value
  x
})

##############################

#' @exportMethod multicore
#' @exportMethod multicore<-
NULL



setGeneric("multicore", function(x) standardGeneric("multicore"))
setGeneric("multicore<-", function(x, value) standardGeneric("multicore<-"))

#' @rdname session
setMethod("multicore", "session", function(x) x@multicore)

#' @rdname session
setReplaceMethod("multicore", signature=c(x="session", value="logical"), function(x, value) {
  x@multicore <- value
  x
})

##############################

#' @exportMethod cores
#' @exportMethod cores<-
NULL



setGeneric("cores", function(x) standardGeneric("cores"))
setGeneric("cores<-", function(x, value) standardGeneric("cores<-"))

#' @rdname session
setMethod("cores", "session", function(x) x@cores)

#' @rdname session
setReplaceMethod("cores", signature=c(x="session", value="numeric"), function(x, value) {
  x@cores <- value
  x
})



##############################

#' @exportMethod smtpServer
#' @exportMethod smtpServer<-
NULL



setGeneric("smtpServer", function(x) standardGeneric("smtpServer"))
setGeneric("smtpServer<-", function(x, value) standardGeneric("smtpServer<-"))

#' @rdname session
setMethod("smtpServer", "session", function(x) x@smtpServer)

#' @rdname session
setReplaceMethod("smtpServer", signature=c(x="session", value="character"), function(x, value) {
  x@smtpServer <- value
  x
})

##############################

#' @exportMethod smtpPort
#' @exportMethod smtpPort<-
NULL



setGeneric("smtpPort", function(x) standardGeneric("smtpPort"))
setGeneric("smtpPort<-", function(x, value) standardGeneric("smtpPort<-"))

#' @rdname session
setMethod("smtpPort", "session", function(x) x@smtpPort)

#' @rdname session
setReplaceMethod("smtpPort", signature=c(x="session", value="character"), function(x, value) {
  x@smtpPort <- value
  x
})

######################

#' @exportMethod email
#' @exportMethod email<-
NULL



setGeneric("email", function(x) standardGeneric("email"))
setGeneric("email<-", function(x, value) standardGeneric("email<-"))

#' @rdname session
setMethod("email", "session", function(x) x@email)

#' @rdname session
setReplaceMethod("email", signature=c(x="session", value="character"), function(x, value) {
  x@email <- value
  x
})


######################

#' @rdname session
setMethod("show", "session", function(object){
  cat("session settings:\n")
  cat("-----------------\n")
  for (x in slotNames(object)){
    cat(paste(sprintf("%-22s", paste(x, ":", sep="")), paste(slot(object, x), collapse=" / "), "\n"))
  }
})
