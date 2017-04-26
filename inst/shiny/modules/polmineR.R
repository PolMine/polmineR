#' Shiny apps and widgets for polmineR.
#' 
#' @export polmineR
#' @rdname polmineR_gui
#' @import polmineR
#' @importFrom shiny icon actionButton selectInput textInput uiOutput radioButtons observeEvent observe withProgress renderUI stopApp runGadget runApp
#' @importFrom shiny isolate updateTextInput updateSelectInput br updateNavbarPage conditionalPanel sliderInput textOutput
#' @importFrom shiny fillPage fillRow fillCol div paneViewer browserViewer shinyUI renderPlot shinyApp tagList fluidPage sidebarLayout sidebarPanel mainPanel
#' @importFrom shinythemes shinytheme
#' @importFrom miniUI miniPage miniContentPanel gadgetTitleBar miniTitleBarCancelButton miniTitleBarButton
polmineR <- function(){
  # shiny::runApp("/Users/blaette/Lab/github/polmineR/inst/shiny/gui")
  runApp(system.file("shiny", package = "polmineR.shiny"))
}