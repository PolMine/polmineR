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
  consoleEncoding="UTF-8"
)
