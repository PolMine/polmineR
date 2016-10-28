#' @export partitionGadget
partitionGadget <- function(){
  if (require("miniUI", quietly = TRUE) && require("shinythemes", quietly = TRUE) && require("shiny", quietly = TRUE)){
    partitionGadgetUI <- miniPage(
      theme = shinytheme("cerulean"),
      gadgetTitleBar(
        "Create partition",
        left = miniTitleBarCancelButton(),
        right = miniTitleBarButton(inputId = "partition_done", label = "Go", primary = TRUE)
      ),
      miniContentPanel(
        fillPage(
          fillRow(
            fillCol(
              div(polmineR:::.partitionUiInput()[["corpus"]],
                  polmineR:::.partitionUiInput()[["name"]],
                  polmineR:::.partitionUiInput()[["pAttribute"]],
                  polmineR:::.partitionUiInput()[["regex"]],
                  polmineR:::.partitionUiInput()[["xml"]]
              )
            ),
            fillCol(br()),
            fillCol(
              div(
                polmineR:::.partitionUiInput()[["sAttributesA"]],
                polmineR:::.partitionUiInput()[["sAttributesB"]]
              )
            ),
            flex = c(1,0.1, 1)
          )
          
        ),
        padding = 10
      )
    )
    
    returnValue <- runGadget(
      app = shinyApp(
        ui = partitionGadgetUI,
        server = polmineR:::.partitionServer
      ),
      viewer = paneViewer()
    )
    return(returnValue)
    
  } else {
    warning("miniUI and/or shinythemes not installed")
  }
}

