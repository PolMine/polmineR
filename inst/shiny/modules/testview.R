# # Convert the file into dataframe
# 
# # Load the package required to read JSON files.
# install.packages("rjson")
# library("rjson")
# library("stringr")
# # Give the input file name to the function.
# result <- fromJSON(file = "/Users/sanikakatekar/Downloads/polmineR-master/inst/shiny/modules/ČKV_ŠURAN.json")
# 
# # Convert JSON file to a data frame.
# json_data_frame <- as.data.frame(result)
# 
# #print(json_data_frame)
# json_conatins<-result [['contains']]
# print(json_conatins)
# 
# #word_input="dragi"
# for (each in seq_along(result[['contains']]))
# {
#   for (i in seq_along(result[['contains']][[each]][['first']][['items']]))
#   {
#     sentence <- result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]
#     cat(paste(each,i,sentence),sep="\n" ,file= "testviewop",append = TRUE)
#     if (grepl("dragi",sentence)==TRUE)
#     {
#       
#       cat(paste(each,i,sentence,regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))[[1]]),sep="\n",file= "testviewop200",append = TRUE)
# 
#     }
#   }
# }
# 
# 
# for (i in seq_along(result[['contains']][[14]][['first']][['items']]))
# {
#   print(result[['contains']][[14]][['first']][['items']][[i]][['body']][['value']])
#  
#   
# }
# 
# #********************************************************************************************************
# install.packages("lexRankr")
# install.packages("tidytext")
# install.packages("readr")
# library(lexRankr)
# library(tidytext)
# library(readr)
# token_i_list<-list()
# sen_tokens<-list()
# sentence_label<-c()
#   for (each in seq_along(result[['contains']]))
#   {
#     for (i in seq_along(result[['contains']][[each]][['first']][['items']]))
#     {
#       sentence <- result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]
#       if (grepl("dragi",sentence)==TRUE)
#       {
#         sentence_label<-regmatches(result[["contains"]][[each]][["label"]],gregexpr(".*_",result[["contains"]][[each]][["label"]]))
#         sentence_label<-paste(unlist(sentence_label),collapse="")
#         cat(paste(each,i,sentence,regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))[[1]]),sep="\n",file= "testviewop20000",append = TRUE)
#         sen_tokens<- append(sen_tokens,sentenceTokenParse(sentence, docId = "create", removePunc = FALSE,
#                                         removeNum = FALSE, toLower = FALSE, stemWords = FALSE,
#                                         rmStopWords = FALSE)$tokens$token)
#        
#         cat(paste((sentenceTokenParse(sentence, docId = "create", removePunc = FALSE,
#                                       removeNum = FALSE, toLower = FALSE, stemWords = FALSE,
#                                       rmStopWords = FALSE)$tokens$token)),sep="   ",file= "testviewop20000",append = TRUE)
#         cat(paste("\n"),sep="\n",file= "testviewop20000",append = TRUE)
#       }
#       print(typeof(regmatches(result[["contains"]][[each]][["label"]],gregexpr(".*_",result[["contains"]][[each]][["label"]]))))
#       # if (regmatches(result[["contains"]][[each]][["label"]],gregexpr(".*_",result[["contains"]][[each]][["label"]]))==sentence_label)
#       # {
#       #   label_eng<-paste(sentence_label,"GLOSS")
#       #   label_word<-paste(sentence_label,"WORD")
#       #   if (result[["contains"]][[each]][["label"]]==label_word)
#       #   {
#       #     #cat(paste(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]),sep="   ",file= "testviewop20000",append = TRUE)
#       #     for (each_tok in sen_tokens)
#       #     {
#       #       if (sentence==each_tok)
#       #       {
#       #         token_i_list <- append(token_i_list, i)
#       #       }
#       #     }
#       #   }
#       #   if (result[["contains"]][[each]][["label"]]==label_eng)
#       #   {
#       #     for (j in token_i_list)
#       #     {
#       #       cat(paste(result [['contains']][[each]][['first']][['items']][[j]][['body']][['value']]),sep="   ",file= "testviewop20000",append = TRUE)
#       # 
#       #     }
#       #   }
#       # 
#       # }
#       
#     }
#   }
# 
# 
# 
# for (i in seq_along(result[['contains']][[14]][['first']][['items']]))
# {
#   print(result[['contains']][[14]][['first']][['items']][[i]][['body']][['value']])
#   
#   
# }

#********************************************************************************************************
install.packages("rjson")
install.packages("lexRankr")
install.packages("tidytext")
install.packages("readr")
install.packages("kable")



library(kable)
library(lexRankr)
library(tidytext)
library(readr)
library(rjson)
library(stringr)
library(dplyr)
library(tidyverse)


# Give the input file name to the function.
result <- fromJSON(file = "/Users/sanikakatekar/Downloads/polmineR-master/inst/shiny/modules/ČKV_ŠURAN.json")

token_i_list<-list()
sen_tokens<-list()
sentence_label<-c()
# df_table<-data.frame(sentence=character(),words=character())
df_engvector<- data.frame(eng_wrds=character())
df_lfeatsvector<- data.frame(wrd_feats=character())




for (each in seq_along(result[['contains']]))
{
  for (i in seq_along(result[['contains']][[each]][['first']][['items']]))
  {
    
    if (grepl("dragi",result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']])==TRUE)
    {
      
      sentence_label<-regmatches(result[["contains"]][[each]][["label"]],gregexpr(".*_",result[["contains"]][[each]][["label"]]))
      sentence_label<-paste(unlist(sentence_label),collapse="")
      
      sentence_label_text<-paste(sentence_label,"TEXT",sep="")
      
      if (result[["contains"]][[each]][["label"]]==sentence_label_text)
      {
        
        sentence <- result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]
        timestamps <- regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))
        timestamps<-paste(unlist(timestamps),collapse="")
        print(timestamps)
        timestart <- regmatches(timestamps, gregexpr('={0}\\d+.{1}\\d+', timestamps))
        tstrt<-as.numeric(timestart[[1]][1])
        tstop<- as.numeric(timestart[[1]][2])
        
        cat(paste(sentence,regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))[[1]]),sep="\n",file= "testviewop80000.txt",append = TRUE)
        df_sentence<-sentence
        sen_tokens<- append(sen_tokens,sentenceTokenParse(sentence, docId = "create", removePunc = FALSE,
                                                          removeNum = FALSE, toLower = FALSE, stemWords = FALSE,
                                                          rmStopWords = FALSE)$tokens$token)
        cat(paste((sentenceTokenParse(sentence, docId = "create", removePunc = FALSE,
                                      removeNum = FALSE, toLower = FALSE, stemWords = FALSE,
                                      rmStopWords = FALSE)$tokens$token)),sep="   ",file= "testviewop80000.txt",append = TRUE)
        df_words<-c((sentenceTokenParse(sentence, docId = "create", removePunc = FALSE,
                                              removeNum = FALSE, toLower = FALSE, stemWords = FALSE,
                                              rmStopWords = FALSE)$tokens$token))
        #df_engvector <- character(length(df_words))
        # df_engvector<- data.frame(eng_wrds=character())
        # # #df_lfeatsvector <- character(length(df_words))
        # df_lfeatsvector<- data.frame(wrd_feats=character())
        #cat(paste("\n"),sep="\n",file= "testviewop80000.txt",append = TRUE)
        df_table<- data.frame(sentence=df_sentence,words= df_words)
      }
      cat(paste("\n"),sep="\n",file= "testviewop80000.txt",append = TRUE)
    }
    sentence_label_eng<-paste(sentence_label,"GLOSS",sep="")
    
    if (result[["contains"]][[each]][["label"]]==sentence_label_eng)
    {
      timestamps_eng <- regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))
      timestamps_eng<-paste(unlist(timestamps_eng),collapse="")
      #print(timestamps_eng)
      timestart_eng <- regmatches(timestamps_eng, gregexpr('={0}\\d+.{1}\\d+', timestamps_eng))
      tstrt_eng<-(as.numeric(timestart_eng[[1]][1]))
      #print(tstrt_eng)
      
      if (tstrt_eng>=tstrt & tstrt_eng<tstop)
      {
        
        #print(paste(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']], tstrt_eng))
        cat(paste(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]),sep="   ",file= "testviewop80000.txt",append = TRUE)
        cat(paste("        "),sep="        ",file= "testviewop80000.txt",append = TRUE)
        #df_engvector <- c(df_engvector, c(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]))
        #df_table$eng_words<- df_engvector[2:length(df_engvector)]
        df_engvector<- rbind(df_engvector,result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']])
        df_table<- data.frame(sentence=df_sentence,words= df_words, eng_vectors= df_engvector)
      }
      if (tstrt_eng==tstop)
      {
        cat(paste("\n"),sep="\n",file= "testviewop80000.txt",append = TRUE)
      }
      
    }
    
    sentence_label_feats<-paste(sentence_label,"FEATS",sep="")
    if (result[["contains"]][[each]][["label"]]==sentence_label_feats)
    {
      
      timestamps_feats <- regmatches(result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]], gregexpr("#t.*", result [['contains']][[each]][["first"]][["items"]][[i]][["target"]][["id"]]))
      timestamps_feats<-paste(unlist(timestamps_feats),collapse="")
      #print(timestamps_eng)
      timestart_feats <- regmatches(timestamps_feats, gregexpr('={0}\\d+.{1}\\d+', timestamps_feats))
      tstrt_feats<-(as.numeric(timestart_feats[[1]][1]))
      #print(tstrt_eng)
      if (tstrt_feats>=tstrt & tstrt_feats<tstop)
      {
        
        #print(paste(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']], tstrt_eng))
        cat(paste(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]),sep="   ",file= "testviewop80000.txt",append = TRUE)
        cat(paste("        "),sep=" ",file= "testviewop80000.txt",append = TRUE)
        #df_lfeatsvector <- c(df_lfeatsvector , c(result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']]))
         #df_table$wrd_feats<- df_lfeatsvector[2:length(df_lfeatsvector)]
        df_lfeatsvector<- rbind(df_lfeatsvector,result [['contains']][[each]][['first']][['items']][[i]][['body']][['value']])
      }
      
      
    }
    
    #df_table<-cbind(df_table,df_engvector) 
    
    #cat(paste("\n"),sep="\n",file= "testviewop80000.txt",append = TRUE)
    # label_eng<-paste(sentence_label,"GLOSS",sep="")
    # label_word<-paste(sentence_label,"WORD",sep="")
    # 
    # if (result[["contains"]][[each]][["label"]]==label_word)
    # {
    #   for (each_tok in sen_tokens)
    #   {
    #     if (result[['contains']][[each]][['first']][['items']][[i]][['body']][['value']]==each_tok)
    #     {
    #       token_i_list <- append(token_i_list, i)
    #       # print(paste(each,i,each_tok))
    #     }
    #   }
    # }
    # if (result[["contains"]][[each]][["label"]]==label_eng)
    # {
    #   for (j in token_i_list)
    #   {
    #     cat(paste(result[['contains']][[each]][['first']][['items']][[j]][['body']][['value']]),sep="   ",file= "testviewop80000.txt",append = TRUE)
    #     
    #   }
    # }
    # 
  }
}










#    tabPanel(
#      "View",
#      sidebarLayout(
#        sidebarPanel = sidebarPanel(
#          tags$audio(src = "Čakaviana_Noel_Šuran_1_mono.wav", type = "audio/mp3", controls = 'play' ),
#          viewUiInput()),
#        mainPanel = mainPanel(viewUiOutput(),
#          tags$table(tags$style(
#
#           'thead {
#            display: table-header-group;
#            vertical-align: middle;
#            border-color: inherit;
#            }
#
#            tr:nth-child(1) {
#            border: solid thick;
#            }
#
#            tr:nth-child(2) {
#            border: solid thick;
#            }
#
#            th {
#            text-align: center;
#            }
#
#            td, th {
#            outline: none;
#            }
#
#            table { 
#            display: table;
#            border-collapse: separate;
#            white-space: normal;
#            line-height: normal;
#            font-family: times-new-roman;
#            font-weight: normal;
#            font-size: medium;
#            font-style: normal;
#            color: -internal-quirk-inherit;
#            text-align: start;
#            border-spacing: 2px;
#            border-color: grey;
#            font-variant: normal;
#            }  
#
#            td {
#            display: table-cell;
#            vertical-align: inherit;
#            }
#
#            tr {
#            display: table-row;
#            vertical-align: inherit;
#            }'
#          )),
#          
#          tags$table(border = 10, 
#                               tags$thead(
#                                tags$tr(
#                                tags$th(colspan = 5, height = 100, width = 1200,align = "center", "GLOSS"))), 
#            tags$tbody(
#            tags$tr(
#              tags$td(align="left","Dobar "),
#              tags$td(align="left","vam "),
#              tags$td(align="left","dan "),
#              tags$td(align="left","dragi "),
#              tags$td(align="left","naši "),
#              ),
#            tags$tr(  
#             tags$td(align="left","good "),
#              tags$td(align="left","you "),
#              tags$td(align="left","day "),
#              tags$td(align="left","dear "),
#              tags$td(align="left","our "),
#              ),
#            tags$tr(  
#              tags$td(align="left","Masc.Nom.Sing "),
#              tags$td(align="left","Prs.2Sing.Dat "),
#              tags$td(align="left","Masc.Nom.Sing "),
#              tags$td(align="left","Masc.Nom.Plur "),
#              tags$td(align="left","Poss.1Plur.Masc.Nom.Plur "),
#            ),
#          tags$tr(tags$td(colspan=9,"Good day to you, our dear [listeners] "))
#            )
#          )
#        )
#       )
#      ),


read.csv("testviewop80000.txt", header = FALSE) %>%
  kable() %>%
  kable_styling(bootstrap_options = "bordered",
                full_width = FALSE)

