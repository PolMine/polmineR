library(shiny)
library(driller)

partitionObjects <- driller:::.getClassObjects('.GlobalEnv', 'partition')

shinyServer(function(input, output, session) {
  observe({
    p <- partitionObjects[[input$partitionObject]]
    updateSelectInput(session, "dim", choices=cqi_attributes(p@corpus, 's'))
  })
  data <- reactive({
    queries <- unlist(strsplit(input$queries, ','))
    queries <- vapply(queries, function(x) gsub('^\\s+', '', x), FUN.VALUE="character", USE.NAMES=FALSE)
    queries <- vapply(queries, function(x) gsub('\\s+$', '', x), FUN.VALUE="character", USE.NAMES=FALSE)
    aha <- dispersion(
      partition=partitionObjects[[input$partitionObject]],
      query=queries,
      dim=input$dim,
      pAttribute=input$pAttribute
      )
    aha
  })
  output$abs <- renderDataTable({
    tab <- data()$abs
    tab <- cbind(rownames(tab), tab)
    tab
    })
  output$rel <- renderDataTable({
    tab <- data()$rel
    tab <- round(tab*100000,2)
    tab <- cbind(rownames(tab), tab)
    tab
  })
  output$plot <- renderPlot({
    if (input$barplot=="rel") {
     barplot(data()$rel, main="relative frequencies")
    } else if (input$barplot=="abs") {
     barplot(data()$abs, main="absolute frequencies")
    }
   })  
})
