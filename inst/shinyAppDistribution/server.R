library(shiny)
library(driller)

partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyServer(function(input, output, session) {
  observe({
    p <- partitionObjects[[input$partitionObject]]
    updateSelectInput(session, "dim", choices=cqi_attributes(p@corpus, 's'))
  })
  data <- reactive({
    aha <- dispersion(
      partition=partitionObjects[[input$partitionObject]],
      query=input$query,
      dim=input$dim,
      pAttribute=input$pAttribute
      )
    aha
  })
  output$tab <- renderDataTable({
    tab <- data()
    tab[,"rel"] <- round(tab[,"rel"]*100000, 2)
    tab <- cbind(tmp=rownames(tab), tab)
    rownames(tab)[1] <- input$dim
    tab
    })
  output$plot <- renderPlot({
    par(mfrow=c(1,2), las=2, mar=c(10,2.5,3,0.5))
    barplot((data()$rel*100000), main="relative frequencies")
    barplot(data()$abs, main="absolute frequencies")
    })  
})
