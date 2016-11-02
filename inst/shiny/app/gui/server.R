shinyServer(function(input, output, session) {

  partitionServer(input, output, session)
  kwicServer(input, output, session)
  contextServer(input, output, session)
  dispersionServer(input, output, session)
  readServer(input, output, session)

})
