library(shiny)
library(polmineR)
library(shinythemes)

# partitionNames <- c(
#   polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'partition'),
#   polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'pressPartition'),
#   polmineR:::.getClassObjectsAvailable('.GlobalEnv', 'plprPartition')
# )
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