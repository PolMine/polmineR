library(shiny)
library(polmineR)
library(magrittr)

# partitionObjects <- polmineR.shiny:::.getClassObjects('.GlobalEnv', 'partition')
controls <- get('session', '.GlobalEnv')

.cgiArguments <- function(sAttr){
  raw <- paste(unlist(lapply(names(sAttr), function(x) paste(x, sAttr[x], sep="="))), collapse="__")
  final <- gsub("\\s", "+", raw)
  return(final)
}

shinyServer(function(input, output, session) {
#  loadedPartition <- load(file.path(session@partitionDir, input$partitionObject, ".RData", sep=""))
  # observe({
  #   foo <- input$partitionButton
  #   availablePartitions <- polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition')
  #   updateSelectInput(session, "partitionObject", choices=availablePartitions)
  # })
  observe({
    x <- input$partitionObject
    new_sAttr <- sAttributes(get(x, ".GlobalEnv")@corpus)
    new_pAttr <- pAttributes(get(x, ".GlobalEnv")@corpus)
    updateSelectInput(session, "pAttribute", choices=new_pAttr, selected=NULL)
    updateSelectInput(session, "meta", choices=new_sAttr, selected=NULL)
  })
  output$query <- renderText({paste('Query: "', input$node, '"', sep='')})
  
  output$table <- DT::renderDataTable({
    input$goButton
    
    withProgress({
      incProgress(0.5, detail = paste("Doing part", 1))
      isolate(
      kwicObject <<- kwic(
        .Object=get(input$partitionObject, '.GlobalEnv'),
        query=input$node, pAttribute=input$pAttribute,
        left=input$leftContext, right=input$rightContext,
#        neighbor=input$collocate,
        meta=input$meta, verbose=FALSE
      )
    )
    }, message("may take a moment"), value=0)
    tab <- kwicObject@table
    if (length(input$meta) > 0){
      print(input$meta)
      metaRow <- unlist(lapply(
        c(1:nrow(tab)),
        function(i){
          sAttr <- unlist(lapply(tab[i,c(1:length(input$meta))], as.character))
          shownText <- paste(sAttr, collapse=" | ")
          wrappedText <- shownText
#           wrappedText <- paste(
#             '<a href="http://localhost/cgi-bin/R/kwic2fulltext?', .cgiArguments(sAttr), '" target="_blank">', shownText, '</a>',
#             sep=''
#           )
          return(wrappedText)
        }
      ))
      if (length(input$meta) > 1){
        retval <- data.frame(
          metadata=metaRow,
          tab[,(length(input$meta)+1):ncol(tab)]
        )
      } else if (length(input$meta) == 1){
        retval <- tab
      }
    } else if (length(input$meta) == 0){
      retval <- data.frame(
        no=c(1:nrow(tab)),
        tab
      )
    }
    retval
  }
  , options=list(
    aoColumnDefs = list(
      list(sClass="metadata", aTargets=c(list(0))),
      list(sClass="alignRight", aTargets=c(list(1))),
      list(sClass="alignCenter", aTargets=c(list(2))),
      list(sWidth="50px", aTargets=c(list(2)))
    )
  ), selection="single"
  )
  # %>% DT::formatStyle("node", color="blue", textAlign="center") %>% DT::formatStyle("left", textAlign="right")
  
  observe({
    input$table_rows_selected
    if (length(input$table_rows_selected) > 0){
      if (input$read == "TRUE"){
        fulltext <- html(kwicObject, input$table_rows_selected, type="plpr")
        browse(fulltext)
      } else {
        output$fulltext <- renderText("you do not want to read")
      }
    }
  })

})
