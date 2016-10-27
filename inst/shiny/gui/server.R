shinyServer(function(input, output, session) {

  polmineR:::.partitionServer(input, output, session)
  polmineR:::.kwicServer(input, output, session)
  polmineR:::.contextServer(input, output, session)
  polmineR:::.dispersionServer(input, output, session)

})
