library(shiny)
library(polmineR)

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
    input$kwic_go
    print(class(get(input$kwic_partition, '.GlobalEnv')))

    isolate(
      kwicObject <<- polmineR::kwic(
        .Object=get(input$kwic_partition, '.GlobalEnv'),
        query=input$kwic_query, pAttribute=input$kwic_pAttribute,
        left=input$kwic_left, right=input$kwic_right,
        meta=input$kwic_meta, verbose=FALSE
      )
    )

    tab <- kwicObject@table
    if (length(input$kwic_meta) > 0){
      print(input$kwic_meta)
      metaRow <- unlist(lapply(
        c(1:nrow(tab)),
        function(i){
          paste(unlist(lapply(tab[i,c(1:length(input$kwic_meta))], as.character)), collapse=" | ")
        }
      ))
      if (length(input$kwic_meta) > 1){
        retval <- data.frame(
          metadata=metaRow,
          tab[,(length(input$kwic_meta)+1):ncol(tab)]
        )
      } else if (length(input$kwic_meta) == 1){
        retval <- tab
      }
    } else if (length(input$kwic_meta) == 0){
      retval <- data.frame(
        no=c(1:nrow(tab)),
        tab
      )
    }
    retval
  } , selection="single"
  )

  observe({
    input$kwic_table_rows_selected
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
  
  output$context_table <- renderDataTable({
    input$context_go
    isolate(
      ctext <- context(
        .Object=get(input$context_partition, '.GlobalEnv'),
        query=input$context_node, pAttribute=input$context_pAttribute,
        left=input$context_left, right=input$context_right,
        verbose=TRUE
      )
    )
    as.data.frame(round(ctext, 2)@stat)
  })

  
})
