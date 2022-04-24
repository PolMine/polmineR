# system requirement: ghostscript (brew install ghostscript)
# the dot program needs to be on the path. Consider adding
# PATH=/opt/homebrew/bin
# to ~/.Renviron

library(polmineR)
library(ruml) # available at/install from: https://github.com/PolMine/ruml
library(plantuml) # available at/install from: https://github.com/rkrug/plantuml
library(magrittr)

plantuml_code <- make_plantuml_code("corpus", pkg = polmineR)
plantuml_obj <- plantuml(plantuml_code)
svglite::xmlSVG(plot(plantuml_obj, vector = TRUE)) %>%
  svgPanZoom::svgPanZoom(width = "100%", height = "100%")

# the output html file and libs are in a temporary directory to be copied to 
# a permanent location