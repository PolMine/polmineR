#' global controls
#' 
#' These controls are used as default by several functions. Compare the rgl package 
#' for a similar approach for controls.
#' 
#' @rdname drillingControls
#' @name drillingControls
#' @export
drillingControls <- list(
  defaultCorpus="PLPRBTTXT",
  pAttribute="word",
  leftContext=5,
  rightContext=5,
  minSignificance=3.84,
  minFrequency=5,
  posFilter=c("NN"),
  filterType="include",
  kwicMetadata=c("text_party"),
  kwicNo=10,
  xtermStyle=FALSE,
  xtermFgNode="red",
  xtermBgNode="white",
  xtermFgCollocate="green",
  xtermBgCollocate="black",
  xtermFgMeta="white",
  xtermBgMeta="blue",
  metadata = c("text_party", "text_speaker", "text_date"),
  multicore=TRUE,
  consoleEncoding="UTF-8",
  smtpServer="mailout.uni-due.de",
  smtpPort="587",
  email="polmine@uni-due.de"
)

#' inspect or set drillingControls
#' 
#' Set drillingControls (a list in the global environment). The controls are used
#' by several functions to keep the number of needed parameters short.
#' WARNING: Providing a character string length > 1 does not yet work!
#' 
#' @param ... parameters you want to set
#' @examples
#' controls() # view the current setting of parameters
#' @rdname controls
#' @name controls
#' 
#' @export
controls <- function(...){
  toSet <- as.list(sys.call())
  if (is.null(names(toSet))){
    foo <- lapply(
      names(drillingControls),
      function(x)
        cat(sprintf("%-20s", paste(x, ":", sep="")), drillingControls[[x]], "\n")
    )
  } else {
    drillingControls <- get("drillingControls", '.GlobalEnv')
    what <- names(toSet)
    what <- what[!what %in% ""]
    for (setting in what) {
      drillingControls[[setting]] <- toSet[[setting]]
    }
    drillingControls <<- drillingControls
  }
}

