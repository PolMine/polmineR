shinyServer(function(input, output, session) {
  
  corpusServer(input, output, session)
  partitionServer(input, output, session)
  kwicServer(input, output, session)
  cooccurrencesServer(input, output, session)
  dispersionServer(input, output, session)
  featuresServer(input, output, session)
  readServer(input, output, session)
  settingsServer(input, output, session)
  countServer(input, output, session)
  
  session$onSessionEnded(function() {
    funs <- c(
      "cooccurrencesServer",
      "cooccurrencesUiInput",
      "cooccurrencesUiOutput",
      "corpusServer",
      "corpusUiInput",
      "corpusUiOutput",
      "countServer",
      "countUiInput",
      "countUiOutput",
      "dispersionServer",
      "dispersionUiInput",
      "dispersionUiOutput",
      "featuresServer",
      "featuresUiInput",
      "featuresUiOutput",
      "getOptionsPolmineR",
      "kwicServer",
      "kwicUiInput",
      "kwicUiOutput",
      "partitionGadget",
      "partitionServer",
      "partitionUiInput",
      "partitionUiOutput",
      "readServer",
      "readUiInput",
      "readUiOutput",
      "rectifySpecialChars",
      "settingsServer",
      "settingsTable",
      "settingsUiInput",
      "settingsUiOutput"
    )
    rm(list = funs, envir = .GlobalEnv)
  })
})
