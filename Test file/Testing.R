library(shiny)

#runExample("01_hello")

# 1. User Interface (UI) - This defines how the app LOOKS
ui <- fluidPage(
  #titlePanel("My first Hello World in Shiny"),
  
  mainPanel(
    # This is the placeholder where the server will send the text
    textOutput("greeting")
  )
)

# 2. Server Logic - This defines what the app DOES
server <- function(input, output) {
  
  # This links to the 'greeting' placeholder in the UI
  output$greeting <- renderText({
    "Hello World"
  })
}

# 3. Run the Application
shinyApp(ui = ui, server = server)