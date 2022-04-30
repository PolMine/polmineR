




#' @rdname shiny_helper_functions
#' @export viewUiInput
#' 

#Input function
viewUiInput <- function(){
 
  list(
    go = actionButton("view_go", label = "", icon = icon("play", lib = "glyphicon")),
    code = actionButton("view_code", label = "", icon = icon("code", lib = "font-awesome")),
    radioButtons("view_object", "class", choices = list("corpus", "partition"), selected = "corpus", inline = TRUE),

    query_view = textInput("view_query", label = "query", value = "dragi")
  )
}


#' @rdname shiny_helper_functions
#' @export viewUiOutput
viewUiOutput <- function() {
 
  DT::dataTableOutput('view_table')

  
  
  
    
  
}

#' @rdname shiny_helper_functions
#' @export viewServer
viewServer <- function(input, output, session){
  
  #reactive function 
  reactive({
    x <- input$view_corpus
    new_sAttr <- s_attributes(input$view_corpus)
    new_pAttr <- p_attributes(input$view_corpus)
    updateSelectInput(session, "view_p_attribute", choices = new_pAttr, selected = NULL)
  })
  
  #the path from which the json file is used
  result <- fromJSON(file = "C:/Users/Sierra/Desktop/new_polmineR/polmineR-master/inst/shiny/modules/istrian.json")
  
  observeEvent(input$view_go,{
    token_i_list<-list()
    sen_tokens<-list()
    sentence_label<-c()
    
    #initializaing empty data frames
    df_engvector<- data.frame()#eng_wrds=character())
    df_lfeatsvector<- data.frame()#wrd_feats=character())
    df_engtrans<- data.frame()
    
    #loop 1: loops for each item in result[['contains']]
    for (each in seq_along(result[['contains']]))
    {
      for (i in seq_along(result[['contains']][[each]][['first']][['items']])) #loop 2: loops inside each result[['contains']] and finds 'items'
      {
        #checks if the query we input exists in the 'value' attribute of 'items' 
        if (grepl(input$view_query,result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']])==TRUE)
        {

          #if query exists: sentence_label will hold the label of the dictionary.
          sentence_label<-regmatches(result[["contains"]][[each]][["label"]],gregexpr(".*_",result[["contains"]][[each]][["label"]]))
          sentence_label<-paste(unlist(sentence_label),collapse="")
          print(sentence_label)
          sentence_label_text<-paste(sentence_label,"TEXT",sep="")
          
          #checks if the label of the current dictionary in loop matches the sentence label
          if (result[["contains"]][[each]][["label"]]==sentence_label_text)
          {
            #if yes, then sentence is displayed
            sentence <- result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]
            #timestamps are extracted from the sentence
            timestamps <- regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))
            timestamps<-paste(unlist(timestamps),collapse="")
            timestart <- regmatches(timestamps, gregexpr('={0}\\d+.{1}\\d+', timestamps))
            #the start timestamp
            tstrt<-as.numeric(timestart[[1]][1])
            #the stop timestamp
            tstop<- as.numeric(timestart[[1]][2])
            
            cat(paste("----------------------------------------------------------------------------------"),sep="\n",file= "shinytxtfile",append = TRUE)
            cat(paste(sentence,regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))[[1]]),sep="\n",file= "shinytxtfile",append = TRUE)
            df_sentence<-sentence #stores the sentence to be displayed
            cat(paste("----------------------------------------------------------------------------------"),sep="\n",file= "shinytxtfile",append = TRUE)
            sen_tokens<- append(sen_tokens,sentenceTokenParse(sentence, docId = "create", removePunc = TRUE,
                                                              removeNum = FALSE, toLower = FALSE, stemWords = FALSE,
                                                          rmStopWords = FALSE)$tokens$token)
           #stores the list of words to be displayed as individual words of the sentence
             df_words<-c((sentenceTokenParse(sentence, docId = "create", removePunc = TRUE,
                                            removeNum = FALSE, toLower = FALSE, stemWords = FALSE,
                                            rmStopWords = FALSE)$tokens$token))
           
            #df_table<- data.frame(sentence=df_sentence,words= df_words)
           
            cat(paste((sentenceTokenParse(sentence, docId = "create", removePunc = TRUE,
                                          removeNum = FALSE, toLower = FALSE, stemWords = FALSE,
                                          rmStopWords = FALSE)$tokens$token)),sep="   ",file= "shinytxtfile",append = TRUE)
            cat(paste("\n"),sep="\n",file= "shinytxtfile",append = TRUE)
            cat(paste("----------------------------------------------------------------------------------"),sep="\n",file= "shinytxtfile",append = TRUE)
            

          }
          cat(paste("\n"),sep="\n",file= "shinytxtfile",append = TRUE)
        }
        #appends the word "GLOSS" to the sentence label to search only through label where our query is present
        sentence_label_eng<-paste(sentence_label,"GLOSS",sep="")
        #if the label is same as the one created above: proceed
        if (result[["contains"]][[each]][["label"]]==sentence_label_eng)
        {
          #gets the timsetamp from this label
          timestamps_eng <- regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))
          timestamps_eng<-paste(unlist(timestamps_eng),collapse="")
          timestart_eng <- regmatches(timestamps_eng, gregexpr('={0}\\d+.{1}\\d+', timestamps_eng))
          tstrt_eng<-(as.numeric(timestart_eng[[1]][1]))
          
          #checks if this timestamp matches the one we extracted above
          #if yes: then display all words in between the start and stop time range as the query word.
          if (tstrt_eng>=tstrt & tstrt_eng<tstop)
          {

            #print the result and append it to text file
            #this text file is only for debugging purpose.
            print(paste(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']], tstrt_eng))
            cat(paste(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]),sep="   ",file= "shinytxtfile",append = TRUE)
            cat(paste("        "),sep="        ",file= "shinytxtfile",append = TRUE)
            
            #the below line directly transfers the result to a dataframe that is displayed in the output
            df_engvector<- rbind(df_engvector,result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']])
           
          }
          #if timestamp reaches the stoptimestamp then stop and append a new line character to the text file
         if(tstrt_eng==tstop)
          {
            cat(paste("\n"),sep="\n",file= "shinytxtfile",append = TRUE)
            cat(paste("----------------------------------------------------------------------------------"),sep="\n",file= "shinytxtfile",append = TRUE)
          }

        }
        #appends the word "FEATS" to the sentence label to search only through label where our query is present
        sentence_label_feats<-paste(sentence_label,"FEATS",sep="")
        #if the label is same as the one created above: proceed
        if (result[["contains"]][[each]][["label"]]==sentence_label_feats)
        {
          #gets the timsetamp from this label
          timestamps_feats <- regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))
          timestamps_feats<-paste(unlist(timestamps_feats),collapse="")
          timestart_feats <- regmatches(timestamps_feats, gregexpr('={0}\\d+.{1}\\d+', timestamps_feats))
          tstrt_feats<-(as.numeric(timestart_feats[[1]][1]))
          
          #checks if this timestamp matches the one we extracted above
          #if yes: then display all words in between the start and stop time range as the query word.
          if (tstrt_feats>=tstrt & tstrt_feats<tstop)
          {
            #print the result and append it to text file
            #this text file is only for debugging purpose.
            cat(paste(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]),sep="   ",file= "shinytxtfile",append = TRUE)
            cat(paste("        "),sep="        ",file= "shinytxtfile",append = TRUE)
            
            #the below line directly transfers the result to a dataframe that is displayed in the output
            df_lfeatsvector<- rbind(df_lfeatsvector,result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']])
            
          }
          #if timestamp reaches the stoptimestamp then stop and append a new line character to the text file
          if(tstrt_feats==tstop)
          {
            cat(paste("\n"),sep="\n",file= "shinytxtfile",append = TRUE)
            cat(paste("----------------------------------------------------------------------------------"),sep="\n",file= "shinytxtfile",append = TRUE)
          }

        }
        #appends the word "TRANS" to the sentence label to search only through label where our query is present
        sentence_label_trans<-paste(sentence_label,"TRANS",sep="")
        #if the label is same as the one created above: proceed
        if (result[["contains"]][[each]][["label"]]==sentence_label_trans)
        {
          timestamps_trans <- regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))
          timestamps_trans<-paste(unlist(timestamps_trans),collapse="")
          timestart_trans <- regmatches(timestamps_trans, gregexpr('={0}\\d+.{1}\\d+', timestamps_trans))
          tstrt_trans<-(as.numeric(timestart_trans[[1]][1]))
          
          #checks if this timestamp matches the one we extracted above
          #if yes: then display all words in between the start and stop time range as the query word.
          if (tstrt_trans==tstrt) #& tstrt_trans<tstop)
          {
            #print the result and append it to text file
            #this text file is only for debugging purpose.
            cat(paste(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]),sep="   ",file= "shinytxtfile",append = TRUE)
            cat(paste("        "),sep="        ",file= "shinytxtfile",append = TRUE)
            
            #the below line directly transfers the result to a dataframe that is displayed in the output
            df_engtrans<- rbind(df_engtrans,result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']])
            
          }
          
          #if timestamp reaches the stoptimestamp then stop and append a new line character to the text file
          if(tstrt_trans==tstop)
           {
             cat(paste("\n"),sep="\n",file= "shinytxtfile",append = TRUE)
             cat(paste("----------------------------------------------------------------------------------"),sep="\n",file= "shinytxtfile",append = TRUE)
          }
          
        }
      }
      
    }
    #this is where the dataframe is created which includes the sentence, english words, features etc.
    df_table<- data.frame(sentence=df_sentence,words= df_words,engWrds= df_engvector,wrdFeats=df_lfeatsvector,trans=df_engtrans)
    #adding column names to the dataframe
    colnames(df_table) <- c('Sentence','Words','EngWords','WordFeatures','Translation')
    #the below command creates a verticalized dataframe 
    df_table_transformed<-as.data.frame(t(df_table))
    
    #send df_table_transformed to output the verticalized version else send df_table to send the horizontal version of the dataframe.
    output$view_table <- DT::renderDataTable({df_table_transformed})
    # output$textWithHTML <- renderUI({
    #   rawText <- readLines('/Users/sanikakatekar/Downloads/polmineR-master/inst/shiny/shinytxtfile') # get raw text
    #   # split the text into a list of character vectors
    #   #   Each element in the list contains one line
    #   splitText <- stringi::stri_split(str = rawText, regex = '\\n')
    #   # wrap a paragraph tag around each element in the list
    #   replacedText <- lapply(splitText,p)
    #   return(replacedText)
    #   
    # })
 }) #end observe-event
  
}# end server function
  
  
#' @rdname polmineR_gui
setMethod("view", "missing", function(){
  
  viewGadgetUI <- shinyUI(fluidPage(
    theme = shinytheme("cerulean"),
    padding = 5,
    gadgetTitleBar(
      "view",
      left = miniTitleBarCancelButton(),
      right = miniTitleBarButton(inputId = "view_go", label = "Go", primary = TRUE)
    ),
    div(br()),
    sidebarLayout(
      sidebarPanel = sidebarPanel(viewUiInput(drop = c("go", "br1", "br2", "p_attribute", "read"))),
      mainPanel = mainPanel(viewUiOutput())
    )
  ))
  
  returnValue <- runGadget(
    app = shinyApp(ui = viewGadgetUI, server = viewServer),
    viewer = browserViewer()
  )
  return(returnValue)
  
}) 

