library(shiny)
library(driller)

partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyServer(function(input, output, session) {
  output$query <- renderText({
    paste('Analysing context for "', input$node, '"', sep='')
     })
  output$frequency <- renderText({
    paste('Node frequency (word):',
          as.character(unlist(partitionObjects[[input$partitionObject]]@tf[[input$pAttribute]][input$node, ][2])))
    })
  output$table <- renderDataTable({
    input$goButton
    isolate(
      ctext <- context(
        partition=partitionObjects[[input$partitionObject]],
        node=input$node,
        pAttribute=input$pAttribute,
        leftContext=input$leftContext,
        rightContext=input$rightContext,
        minSignificance=input$minSignificance,
        posFilter=unlist(strsplit(input$posFilter, ' '))
        )
      )
    cbind(token=rownames(ctext@stat), ctext@stat)
    })
})
