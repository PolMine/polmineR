#' @include S4classes.R
NULL

#' Assign and get labels.
#' 
#' @param x object
#' @param value length (character vector, length 1)
#' @param n label index
#' @param value a value to assign
#' @param ... further parameters
#' @rdname label_method
#' @exportMethod label
setGeneric("label", function(x, ...) standardGeneric("label"))

#' @rdname label_method
setGeneric("label<-", function(x, value) standardGeneric("label<-"))

#' @importFrom utils menu
#' @importFrom DT dataTableProxy replaceData
#' @rdname label_method
#' @examples 
#' o <- kwic("REUTERS", query = "oil")
#' label(o, 1L, FALSE)
#' \dontrun{
#' label(o)
#' }
setMethod("label", "kwic", function(x, n = NULL, value){
  
  if (is.null(n)){
    if (!requireNamespace(package = "shiny", quietly = TRUE))
      stop("Package 'shiny' is required, but not available.")
    
    if (!requireNamespace(package = "miniUI", quietly = TRUE))
      stop("Package 'shiny' is required, but not available.")
    
    ui <- miniUI::miniPage(
      miniUI::gadgetTitleBar(title = "KWIC Labelling Gadget"),
      miniUI::miniContentPanel(
        DT::dataTableOutput('kwic', height = "100%")
      )
    )
    
    kwic_dt <- as.data.table(x@table)[, "hit_no" := NULL][, "label" := x@labels$labels]

    server <- function(input, output, session) {
      
      v <- reactiveValues(data = kwic_dt)
      proxy <- dataTableProxy("kwic")
      
      observeEvent(
        input$kwic_cell_edit,
        {
          i <- input$kwic_cell_edit$row
          j <- input$kwic_cell_edit$col
          k <- input$kwic_cell_edit$value

          isolate(
            if (j == match("label", colnames(v$data))) {
              new_value <- DT::coerceValue(k, v$data[i, colnames(v$data)[j]])
              v$data[i, colnames(v$data)[j]] <<- new_value
              label(x, n = i, value = new_value)
            }
          )
          DT::replaceData(proxy, v[["data"]], resetPaging = FALSE)  # replaces data displayed by the updated table
        }
      )
      
      output$kwic <- DT::renderDataTable(
        DT::datatable(kwic_dt, editable = TRUE, options = list(lengthChange = FALSE)) %>%
          DT::formatStyle("node", color = "blue", textAlign = "center") %>% 
          DT::formatStyle("left", textAlign = "right")
      )
      observeEvent(input$done, shiny::stopApp(v[["data"]]))
    }
    
    y <- shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = "maximize"))
    return(invisible(y))
  } else {
    stopifnot(is.integer(n))
    x@labels$labels[n] <- value
    return(invisible(x))
  }
})

