partitionNames <- c(
  polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition'),
  polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'pressPartition'),
  polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'plprPartition')
)


partitionTabPanel <- function(){
  tabPanel(
    "partition",
    sidebarLayout(
      sidebarPanel(
        actionButton("partition_go", label="", icon=icon("play", lib="glyphicon")), br(),br(),
        selectInput("partition_corpus", "corpus", choices = corpus(), selected = corpus()[1]),
        textInput(inputId = "partition_name", label = "name", value = "FOO"),
        selectInput(
          inputId="partition_sAttributes", label="sAttributes", multiple = TRUE,
          choices=sAttributes(get(partitionNames[1], envir=.GlobalEnv)@corpus)
        ),
        uiOutput("partition_sAttributes"),
        selectInput(inputId="partition_pAttribute", label="pAttribute", multiple = TRUE, choices=list(none = "", word = "word", lemma = "lemma")),
        radioButtons("partition_regex", "regex", choices = list("TRUE", "FALSE"), inline = TRUE),
        radioButtons("partition_xml", "xml", choices = list("flat", "nested"), inline = TRUE)
      ),
      mainPanel(
        dataTableOutput('partition_table')
      )
      
    )
  )
}


partitionServer <- function(input, output, session){
  observeEvent(
    input$partition_go,
    {
      selectInputToUpdate <- c("kwic_partition", "context_partition", "dispersion_partition")
      if (input$partition_go > 0){
        defList <- lapply(
          setNames(input$partition_sAttributes, input$partition_sAttributes),
          function(x) gsub('„', '"', input[[x]]) 
        )
        assign(
          input$partition_name,
          partition(
            as.character(input$partition_corpus),
            def=defList,
            # def=eval(parse(text=paste("list(", def_purged, ")", sep=""))),
            name=input$partition_name,
            pAttribute=input$partition_pAttribute,
            regex=input$partition_regex,
            xml=input$partition_xml,
            mc=FALSE,
            verbose=TRUE
          ),
          envir=.GlobalEnv
        )
        partitionDf <- partition()
        rownames(partitionDf) <- NULL
        output$partition_table <- renderDataTable(partitionDf)
        
        for (toUpdate in selectInputToUpdate) {
          updateSelectInput(session, toUpdate, choices=partitionDf$object, selected=NULL)  
        }
      }
    }
  )
  
  observeEvent(
    input$partition_corpus,
    {
      updateSelectInput(
        session, inputId = "partition_sAttributes",
        choices = sAttributes(input$partition_corpus)
      )
    }
  )
  
  output$partition_sAttributes <- renderUI({
    tagList(lapply(
      input$partition_sAttributes,
      function(x) textInput(x, x)
    ))
  })
  
  
  output$partition_table <- renderDataTable(partition())
  
}



kwicTabPanel <- function(){
  tabPanel(
    "kwic",
    sidebarLayout(
      sidebarPanel(
        actionButton("kwic_go", "Go!"), br(),br(),
        selectInput("kwic_partition", "partition", choices=partitionNames),
        textInput("kwic_query", "query", value="Suche"),
        textInput("kwic_neighbor", "neighbor", value=""),
        selectInput(
          "kwic_meta", "sAttribute",
          choices=sAttributes(get(partitionNames[1], ".GlobalEnv")@corpus),
          multiple=TRUE
        ),
        selectInput(
          "kwic_pAttribute", "pAttribute",
          choices=pAttributes(get(partitionNames[1], ".GlobalEnv")@corpus)
        ),
        numericInput("kwic_left", "left", value=getOption("polmineR.left")),
        numericInput("kwic_right", "right", value=getOption("polmineR.right")),
        radioButtons("kwic_read", "read", choices=c("TRUE", "FALSE"), selected="FALSE", inline=T),
        br()
      ),
      
      mainPanel(DT::dataTableOutput('kwic_table'))
    )
  )
}

kwicServer <- function(input, output, session){
  
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
  
}


contextTabPanel <- function(){
  tabPanel(
    "context",
    sidebarLayout(
      sidebarPanel(
        actionButton("context_go", "Go!"),
        br(), br(),
        selectInput("context_partition", "partition", partitionNames[1]),
        textInput("context_query", "query", value="Suche"),
        selectInput("context_pAttribute", "pAttribute:", choices=c("word", "pos", "lemma"), selected=getOption("polmineR.pAttribute"), multiple=TRUE),
        numericInput("context_left", "left", value=getOption("polmineR.left")),
        numericInput("context_right", "right", value=getOption("polmineR.right")),
        br()
      ),
      
      mainPanel(
        DT::dataTableOutput('context_table'),
        textOutput("context2kwic")
      )
    )
  )
}

contextServer <- function(input, output, session){
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
}

dispersionTabPanel <- function(){
  tabPanel(
    "dispersion",
    sidebarLayout(
      sidebarPanel(
        actionButton("dispersion_go", "Go!"),
        br(), br(),
        selectInput("dispersion_partition", "partition", choices=partitionNames, selected=partitionNames[1]),
        textInput("dispersion_query", "query", value="Suche"),
        selectInput(
          "dispersion_pAttribute", "pAttribute",
          choices=pAttributes(get(partitionNames[1], ".GlobalEnv")@corpus)
        ),
        # radioButtons("dispersion_dims", "number of sAttributes", choices=c("1", "2"), selected="1", inline=TRUE),
        selectInput(
          "dispersion_sAttribute_1", "sAttribute",
          choices=sAttributes(get(partitionNames[1])@corpus), multiple=FALSE
        ),
        radioButtons("dispersion_ts", "time series", choices=c("yes", "no"), selected="no", inline=TRUE),
        conditionalPanel(
          condition = "input.dispersion_ts == 'yes'",
          selectInput("dispersion_ts_aggregation", "aggregation", choices=c("none", "month", "quarter", "year"), multiple = FALSE
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Table",
            DT::dataTableOutput('dispersion_table')
          ),
          tabPanel(
            "Plot",
            plotOutput('dispersion_plot')
          )
        )
        
      )
    )
  )
}

dispersionServer <- function(input, output, session){
  observe({
    x <- input$dispersion_partition
    new_sAttr <- sAttributes(get(x, ".GlobalEnv")@corpus)
    updateSelectInput(
      session, "dispersion_pAttribute",
      choices=pAttributes(get(x, ".GlobalEnv")@corpus), selected=NULL
    )
    updateSelectInput(session, "dispersion_sAttribute_1", choices=new_sAttr, selected=NULL)
    # if (input$dispersion_dims == "2"){
    #   updateSelectInput(session, "dispersion_sAttribute_2", choices=new_sAttr, selected=NULL)
    # }
  })
  
  observeEvent(
    input$dispersion_go,
    {
      #        if (input$dispersion_dims == "1"){
      sAttrs <- input$dispersion_sAttribute_1
      #          print(sAttrs)
      #        } else if (input$dispersion_dims == "2"){
      #          sAttrs <- c(input$dispersion_sAttribute_1, input$dispersion_sAttribute_2)
      #          print(sAttrs)
      #        }
      tab <- as.data.frame(dispersion(
        get(input$dispersion_partition),
        query=input$dispersion_query,
        sAttribute=sAttrs,
        pAttribute=input$dispersion_pAttribute
      ))
      tab[["freq"]] <- round(tab[["freq"]], 7)
      if (input$dispersion_ts == "yes"){
        if (requireNamespace("zoo", quietly=TRUE)){
          if (input$dispersion_ts_aggregation != "none"){
            dates4zoo <- tab[[input$dispersion_sAttribute_1]]
            tab[[input$dispersion_sAttribute_1]] <- NULL
            zooObject <- zoo::zoo(as.matrix(tab), order.by=as.Date(dates4zoo))
            zooObjectAggr <- switch(
              input$dispersion_ts_aggregation,
              month = zoo:::aggregate.zoo(zooObject, zoo::as.Date(zoo::as.yearmon(zoo::index(zooObject)))),
              quarter = zoo:::aggregate.zoo(zooObject, zoo::as.Date(zoo::as.yearqtr(zoo::index(zooObject)))),
              year = zoo:::aggregate.zoo(zooObject, as.Date(paste(gsub("^(\\d{4}).*?$", "\\1", zoo::index(zooObject)), "-01-01", sep="")))
            )
            tab <- data.frame(date=zoo::index(zooObjectAggr), zooObjectAggr)
            colnames(tab)[1] <- input$dispersion_sAttribute_1
            rownames(tab) <- NULL
          }
          output$dispersion_plot <- renderPlot(zoo:::plot.zoo(zooObjectAggr, main=""))
        } else {
          message("package 'zoo' required, but not available")
        }
      } else {
        # output$dispersion_plot <- renderPlot(as.data.frame(tab))
      }
      output$dispersion_table <- DT::renderDataTable(as.data.frame(tab))
      
    }
  )
}