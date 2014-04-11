library(shiny)
library(driller)

partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyServer(function(input, output, session) {
  observe({
    p <- partitionObjects[[input$partitionObject]]
    updateSelectInput(session, "rows", choices=cqi_attributes(p@corpus, 's'))
    updateSelectInput(session, "cols", choices=cqi_attributes(p@corpus, 's'))
  })
  
  output$what <- renderText({
    paste('Analysing distribution', input$rows, input$query, input$cols)
     })
  data <- reactive({
    aha <- dispersion(
      partition=partitionObjects[[input$partitionObject]],
      query=input$query,
      dim=c(input$rows, input$cols),
      pAttribute=input$pAttribute
      )
    aha
  })
  output$tab <- renderDataTable({
    tab <- slot(data(), "total")[[input$what]]
    if (input$what == "rel") {
      tab <- round(tab*100000,2)
    }
    tab <- cbind(rownames(tab), tab)
    tab
    })
   output$plot <- renderPlot({
     bubblegraph(slot(data(), "total")[[input$what]], rex=input$rex)
   })  
})
