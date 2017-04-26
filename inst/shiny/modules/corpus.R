#' @rdname shiny_helper_functions
#' @export corpusUiInput
corpusUiInput <- function(){
  list(
    corpus = selectInput("corpus_corpus", "corpus", choices = corpus()[["corpus"]], selected = corpus()[["corpus"]][1])
  )
}


#' @rdname shiny_helper_functions
#' @export corpusUiOutput
corpusUiOutput <- function(){
}


#' @rdname shiny_helper_functions
#' @export corpusServer
corpusServer <- function(input, output, session){

  observeEvent(
    input$corpus_corpus,
    {
      infoFile <- RegistryFile$new(input$corpus_corpus)$getInfo()
      if (file.exists(infoFile)){
        content <- scan(file = infoFile, what = character(), sep = "\n")
        if (grepl(".md$", infoFile)){
            content <- markdown::markdownToHTML(text = content)
            content <-  htmltools::HTML(gsub("^.*<body>(.*?)</body>.*?$", "\\1", as.character(content)))
        }
      } else {
        content <- htmltools::HTML("</br><i>no corpus info file available</i>")
      }
      output$corpus_info <- renderUI(content)
    }
  )
  
}
