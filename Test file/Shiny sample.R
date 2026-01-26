library(shiny)

ui <- fluidPage(
  numericInput("num_boxes", "How many boxes?", value = 3, min = 1),
  uiOutput("dynamic_ui")
)

server <- function(input, output) {
    output$dynamic_ui <- renderUI({
    # use a loop-like function (lapply) to create multiple components
    lapply(1:input$num_boxes, function(i) {
      textInput(paste0("box_", i), paste("Input Box", i))
    })
  })
}

shinyApp(ui, server)