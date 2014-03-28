library(shiny)
library(driller)

partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyServer(function(input, output) {
  output$c1 <- renderText({input@coi})
  output$c2 <- renderText({input@ref})
  output$table <- renderDataTable({
    input$goButton
    isolate(
      keyness <- keyness(
        coi=partitionObjects[[input$coi]],
        ref=partitionObjects[[input$ref]],
        pattribute=input$pAttribute,
        included=input$included,
        min.significance=input$minSignificance,
        min.frequency=input$minFrequency,
        verbose=FALSE,
        pos.filter=NULL
        )
      )
    cbind(token=rownames(keyness@stat), keyness@stat)
    })
})
