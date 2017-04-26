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
    rm(values, envir = .GlobalEnv)
  })
})
