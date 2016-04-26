library(shiny)
library(polmineR)
library(magrittr)
library(DT)

controls <- get('session', '.GlobalEnv')


shinyServer(function(input, output, session) {

  ## partition
  
  observe({
    input$partition_go
    isolate({
      assign(
        input$partition_name,
        partition(
          as.character(input$partition_corpus),
          def=eval(parse(text=paste("list(", input$partition_def, ")", sep=""))),
          name=input$partition_name,
          pAttribute=input$partition_pAttribute,
          regex=input$partition_regex,
          xml=input$partition_xml,
          mc=FALSE,
          verbose=TRUE
        ),
        envir=.GlobalEnv
      )
      partitionDf <- partition(controls)
      output$partition_table <- renderDataTable(partitionDf)
      updateSelectInput(session, "kwic_partition", choices=partitionDf$object, selected=NULL)
      updateSelectInput(session, "context_partition", choices=partitionDf$object, selected=NULL)
    })
  })
  
  output$partition_table <- renderDataTable(partition(controls))
  
  ## kwic 
  
  observe({
    x <- input$kwic_partition
    new_sAttr <- sAttributes(get(x, ".GlobalEnv")@corpus)
    new_pAttr <- pAttributes(get(x, ".GlobalEnv")@corpus)
    updateSelectInput(session, "kwic_pAttribute", choices=new_pAttr, selected=NULL)
    updateSelectInput(session, "kwic_meta", choices=new_sAttr, selected=NULL)
  })
  
  output$kwic_table <- DT::renderDataTable({
    
    withProgress(
      message="please wait", value=0, max=5, detail="preparing data",
      {
      
      input$kwic_go
      # incProgress(0.5, detail = paste("Doing part", 1))
      
    isolate({
      kwicObject <<- polmineR::kwic(
        .Object=get(input$kwic_partition, '.GlobalEnv'),
        query=input$kwic_query, pAttribute=input$kwic_pAttribute,
        left=input$kwic_left, right=input$kwic_right,
        meta=input$kwic_meta, verbose="shiny",
        neighbor=input$kwic_neighbor
        )
      })
    
    tab <- kwicObject@table
    if (length(input$kwic_meta) == 0){
      retval <- data.frame(no=c(1:nrow(tab)), tab)
    } else if (length(input$kwic_meta) > 0){
      metaRow <- unlist(lapply(
        c(1:nrow(tab)),
        function(i){
          paste(unlist(lapply(tab[i,c(1:length(input$kwic_meta))], as.character)), collapse=" | ")
        }
      ))
      retval <- data.frame(meta=metaRow, tab[,(length(input$kwic_meta)+1):ncol(tab)])
    }
      }) # end withProgress
    retval <- DT::datatable(retval, selection="single", rownames=FALSE) %>% 
      DT::formatStyle("node", color="#4863A0", textAlign="center") %>%
      DT::formatStyle("left", textAlign="right")
    if (length(input$kwic_meta) > 0){
      retval <- DT::formatStyle(retval, "meta", fontStyle="italic", textAlign="left", borderRight="1px solid DarkGray")
    }
    retval
  }
  )

  observeEvent(
    input$kwic_table_rows_selected,
    {
      if (length(input$kwic_table_rows_selected) > 0){
        if (input$kwic_read == "TRUE"){
          fulltext <- html(kwicObject, input$kwic_table_rows_selected, type="plpr")
          browse(fulltext)
        } else {
          output$fulltext <- renderText("you do not want to read")
        }
      }
    })
  
  ## context
  
  output$context_table <- DT::renderDataTable({
    withProgress(
      message="please wait ...", value=0, max=6, detail="getting started",
    {
    input$context_go
    isolate(
      ctext <<- context(
        .Object=get(input$context_partition, '.GlobalEnv'),
        query=input$context_query,
        pAttribute=input$context_pAttribute,
        left=input$context_left, right=input$context_right,
        verbose="shiny"
      )
    )})
    DT::datatable(round(ctext, 2)@stat, selection="single", rownames=FALSE)
  })

  observeEvent(
    input$context_table_rows_selected,
    {
      if (length(input$context_table_rows_selected) > 0){
        updateTextInput(
          session, "kwic_neighbor",
          value = ctext@stat[[input$context_pAttribute[1]]][input$context_table_rows_selected]
        )
        updateSelectInput(session, "kwic_partition", selected = input$context_partition)
        updateTextInput(session, "kwic_query", value = input$context_query)
        updateSelectInput(session, "kwic_left", selected = input$context_left)
        updateSelectInput(session, "kwic_right", selected = input$context_right)
        updateSelectInput(session, "kwic_pAttribute", selected = input$context_pAttribute)
        updateNavbarPage(session, "polmineR", selected = "kwic")  
      }
    })
  
})
