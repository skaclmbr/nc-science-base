library(shiny)
library(jsonlite)

# JSON Editor Module UI
jsonEditorUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("json_file"), "Upload JSON File", accept = ".json"),
    textAreaInput(ns("json_text"), "JSON Content", height = "300px"),
    actionButton(ns("save_json"), "Save JSON"),
    downloadButton(ns("download_json"), "Download JSON"),
    textInput(ns("key"), "Key"),
    selectInput(ns("type"), "Type", choices = c("String", "Number", "Boolean", "List", "Dictionary")),
    textAreaInput(ns("value"), "Value"),
    actionButton(ns("add_entry"), "Add Entry")
  )
}

# JSON Editor Module Server
jsonEditorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    json_data <- reactiveVal(list())
    
    observeEvent(input$json_file, {
      req(input$json_file)
      json_content <- fromJSON(input$json_file$datapath, flatten = TRUE)
      json_data(json_content)
      updateTextAreaInput(session, "json_text", value = toJSON(json_content, pretty = TRUE, auto_unbox = TRUE))
    })
    
    observeEvent(input$save_json, {
      req(input$json_text)
      json_data(fromJSON(input$json_text))
    })
    
    observeEvent(input$add_entry, {
      req(input$key, input$type, input$value)
      new_value <- switch(input$type,
                          "String" = input$value,
                          "Number" = as.numeric(input$value),
                          "Boolean" = as.logical(input$value),
                          "List" = fromJSON(input$value),
                          "Dictionary" = fromJSON(input$value))
      
      updated_json <- json_data()
      updated_json[[input$key]] <- new_value
      json_data(updated_json)
      updateTextAreaInput(session, "json_text", value = toJSON(updated_json, pretty = TRUE, auto_unbox = TRUE))
    })
    
    output$download_json <- downloadHandler(
      filename = function() { "edited_json.json" },
      content = function(file) {
        write(toJSON(json_data(), pretty = TRUE, auto_unbox = TRUE), file)
      }
    )
  })
}

# Main App UI
ui <- fluidPage(
  titlePanel("Modular JSON Editor"),
  jsonEditorUI("json_editor")
)

# Main App Server
server <- function(input, output, session) {
  jsonEditorServer("json_editor")
}

# Run App
shinyApp(ui, server)
