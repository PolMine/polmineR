#' @export getOptionsPolmineR
getOptionsPolmineR <- function(){
  gsub("polmineR\\.", "", grep("polmineR.", names(options()), value = TRUE))
}

#' @rdname shiny_helper_functions
#' @export settingsUiInput
settingsUiInput <- function(){
  list(
    actionButton("settings_go", "", icon = icon("play", lib = "glyphicon")),
    br(), br(),
    selectInput("settings_option", "option", choices = getOptionsPolmineR(), selected = getOptionsPolmineR()[1]),
    textInput("settings_new", "new value", placeholder = "new setting")
  )
}

#' @rdname shiny_helper_functions
#' @export settingsUiOutput
settingsUiOutput <- function(){
  list(
    DT::dataTableOutput('settings_table')
  )
}

settingsTable <- c("foo")

#' @rdname shiny_helper_functions
#' @export settingsServer
settingsServer <- function(input, output, session){
  observe({
    input$settings_go
    isolate({
      newSetting <- list()
      newSetting <- list(input$settings_new)
      names(newSetting) <- paste("polmineR", input$settings_option, sep = ".")
      print(newSetting)
      options(newSetting)
      settingsTable <- data.frame(
        option = getOptionsPolmineR(),
        value = as.character(sapply(getOptionsPolmineR(), function(x) getOption(paste("polmineR", x, sep = "."))))
      )
      output$settings_table <- DT::renderDataTable(settingsTable)
      
    })
  })
}
