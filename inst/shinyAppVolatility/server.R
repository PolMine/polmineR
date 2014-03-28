library(shiny)
library(driller)

partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyServer(function(input, output, session) {
  a <- reactive({
    node <- input$node.a
    Encoding(node) <- "UTF-8"
    input$aButton
    isolate(a <- context(
      partition=partitionObjects[[input$partitionA]],
      node=node,
      pAttribute=input$pattribute,
      leftContext=input$context,
      rightContext=input$context,
      minSignificance=input$significance,
      posFilter=unlist(strsplit(input$pos.filter, '\\s'))
      )
    )
    a
  })
  output$info.a <- renderText({
    paste('Frequeny of "', input$node.a, '": ', as.character(a()@frequency), ' (total context size: ', as.character(a()@size),')', sep='')
  })
  
  b <- reactive({
    node <- input$node.b
    Encoding(node) <- "UTF-8"
    input$bButton
    isolate(
    b <- context(
      partition=partitionObjects[[input$partitionB]],
      node=node,
      pAttribute=input$pattribute,
      leftContext=input$context,
      rightContext=input$context,
      minSignificance=input$significance,
      posFilter=unlist(strsplit(input$pos.filter, '\\s'))
      )
    )
    b
  })
  output$info.b <- renderText({
    paste('Frequeny of "', input$node.b, '": ', as.character(b()@frequency), ' (total context size: ', as.character(b()@size),')', sep='')
  })
  combi <- reactive({
    input$fullButton
    isolate(
      back <- combineCollocates(
        a(),
        b(),
        max.rank=input$max.rank,
        min.frequency=input$min.frequency,
        pearson=ifelse(input$pearson=="Yes", TRUE, FALSE)
        )
    )
    back
  })
  output$tab <- renderDataTable({
    cbind(token=rownames(combi()$stat), combi()$stat)
  })
  output$rho <- renderText({
    if (input$pearson=="Yes") {r <- combi()$pearsons.rho}
    else {r <- 'is not calculated'}
    paste('Pearsons Rho:', r)
  })
  output$scatterplot <- renderPlot({
      scatterplotCollocates(combi(), xmax=input$max.rank, ymax=input$max.rank, fontSize=input$fontSize, rotation=input$rotation)
  }) 
})
