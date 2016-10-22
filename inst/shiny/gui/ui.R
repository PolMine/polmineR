library(shiny)
library(polmineR)
library(shinythemes)

shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean

shinyUI(
  
  navbarPage(
    
    theme = shinyThemeToUse,
    title = "polmineR",
    id = "polmineR",
    
    partitionTabPanel(), # defined in global.R
    kwicTabPanel(),
    contextTabPanel(),
    dispersionTabPanel()

   )
)