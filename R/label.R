#' @include S4classes.R
NULL

#' Assign, set and get labels.
#' 
#' This set of methods supports generating and working with labelled data. They
#' rely on data in a slot \code{labels} of the S4 object to be modified.
#' Operations are deliberately in-place.
#'
#' The \code{label}-method is designed to be used in a RStudio session. Calling
#' it generates a shiny gadget (see
#' \url{https://shiny.rstudio.com/articles/gadgets.html}) shown in the viewer
#' pane of RStudio. The htmlwidget embedded in the gadget can be either based on
#' the JavaScript library DataTables (default, exposed by R package \code{DT}),
#' or Handsontable (exposed by R package \code{rhandsontable}). The JavaScript
#' library to be used is specified with the argument \code{js}.
#'
#' The pinpoint is that you can modify labels. When using DataTables,
#' doubleclick on the field you with to change, modify the value, and finish
#' data entry by hitting enter or clicking somewhere else. When using
#' Handsontable, simply click on the field you wish to change.
#'
#' The method returns the modified input object. Note however that changes of
#' labels are deliberately in-place operations. Accordingly, the input object is
#' changed even if you do not close the gadget "properly" by hitting the "Done"
#' button and catch the modified object, which may be forgotten easily and would
#' be painful after the work that may have been invested.
#' 
#' @param x An S4 object with a slot \code{labels} (e.g. an object of class
#'   \code{kwic}).
#' @param n The integer index of a label to retrieve or modify.
#' @param value A value to assign.
#' @param js The javascript library to use to display tables (either "DataTables"
#'   or "Handsontable").
#' @param ... Further arguments.
#' @rdname label_method
#' @return The modified input object is returned invisibly.
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
#' 
#' \dontrun{
#' label(o, js = "DataTables")
#' slot(o, "labels")[["labels"]] # to see changes made
#' 
#' label(o, js = "Handsontable")
#' slot(o, "labels")[["labels"]] # to see changes made
#' 
#' enrich(o, s_attributes = "places")
#' label(o, js = "Handsontable")
#' }
setMethod("label", "kwic", function(x, n = NULL, value, js = c("DataTables", "Handsontable")){
  
  if (length(js) > 1L) js <- js[1]
  
  if (is.null(n)){
    if (!requireNamespace(package = "shiny", quietly = TRUE))
      stop("Package 'shiny' is required, but not available.")
    
    if (!requireNamespace(package = "miniUI", quietly = TRUE))
      stop("Package 'shiny' is required, but not available.")
    
    kwic_dt <- as.data.table(x@table)[, "hit_no" := NULL][, "label" := x@labels$labels]

    if (js == "DataTables"){
      
      ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar(title = "KWIC Labelling Gadget"),
        miniUI::miniContentPanel(
          DT::dataTableOutput('kwic', height = "100%")
        )
      )

      server <- function(input, output, session) {
        
        v <- shiny::reactiveValues(data = kwic_dt)
        proxy <- DT::dataTableProxy("kwic")
        
        shiny::observeEvent(
          input$kwic_cell_edit,
          {
            i <- input$kwic_cell_edit$row
            j <- input$kwic_cell_edit$col
            k <- input$kwic_cell_edit$value
            
            shiny::isolate(
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
        shiny::observeEvent(input$done, shiny::stopApp(v[["data"]]))
      }
      
      y <- shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = "maximize"))
      return(invisible(y))
      
    } else if (js == "Handsontable"){
      
      kwic_dt[, "label" := as.logical(kwic_dt[["label"]])]
      
      if (!requireNamespace(package = "rhandsontable", quietly = TRUE))
        stop("Package 'rhandsontable' is required, but not available.")
      
      ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar("Edit a data.frame"), 
        miniUI::miniContentPanel(
          rhandsontable::rHandsontableOutput("hot"),
          shiny::uiOutput("pending")
        )
      )
      
      server <- function(input, output, session) {
        
        values <- shiny::reactiveValues()
        
        reset_values <- function(df){
          values[["hot"]] <- df
          x@labels$labels <- as.character(df[["label"]])
        }
        
        reactiveData <- shiny::reactive(kwic_dt)
        
        output$pending <- shiny::renderUI({
          data <- reactiveData()
          if (rhandsontable:::isErrorMessage(data)) htmltools::h4(style = "color: #AA7732;", data$message)
        })
        
        output$hot <- rhandsontable::renderRHandsontable({
          data <- reactiveData()
          if (rhandsontable:::isErrorMessage(data)) return(NULL)
          df <- if (is.null(input$hot)) data else rhandsontable::hot_to_r(input$hot)
          reset_values(df)
          rhandsontable::rhandsontable(df) %>%
            rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            rhandsontable::hot_col(col = "left", halign = "htRight") %>%
            rhandsontable::hot_col(col = "node", halign = "htCenter")
        })
        
        shiny::observeEvent(
          input$done,
          shiny::stopApp(
            # returnValue = values[["hot"]]
            returnValue = x
            )
          )
      }
      
      y <- shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = "maximize"))
      
      return(invisible(y))
      
    }
    
  } else {
    stopifnot(is.integer(n))
    x@labels$labels[n] <- value
    return(invisible(x))
  }
})

