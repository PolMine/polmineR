library(shiny)
library(polmineR)
library(magrittr)

partitionObjects <- polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')
sessionSettings <-  get('session', '.GlobalEnv')

shinyServer(function(input, output, session) {
  observe({
    foo <- input$partitionButton
    availablePartitions <- polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')
    updateSelectInput(session, "partitionObject", choices=availablePartitions)
  })
  output$query <- renderText({
    paste(
      'Query: "',
      input$node, '"',
      sep='')
     })
  output$table <- renderDataTable({
    input$goButton
    isolate(
      ctext <- context(
        .Object=get(input$partitionObject, '.GlobalEnv'),
        query=input$node,
        pAttribute=input$pAttribute,
        left=input$left,
        right=input$right,
        verbose=TRUE
        )
#      %>% subset(ll > input$minSignificance)
      )
    # round(ctext, 2)
    # cbind(token=rownames(ctext@stat), ctext@stat)
    as.data.frame(round(ctext, 2)@stat)
    })
})
