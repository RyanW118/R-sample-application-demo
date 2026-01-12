# ===============================
# R Shiny Multi-Function Demo App
# ===============================

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinyjs)
library(httr)
library(xml2)
library(rvest)
library(stringr)

# ---------- UI ----------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* Background and Global Text */
      body { 
        background-color: #F6F0D7; 
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: #000000 !important;
      }
      
      h1, h2, h3, h4, p, li, span, label, .control-label { 
        color: #000000 !important; 
      }

      /* Hero Section */
      .hero-section {
        text-align: center;
        padding: 80px 20px;
        background-color: #C5D89D;
        border-radius: 0 0 50px 50px;
        margin-bottom: 30px;
        border-bottom: 2px solid #9CAB84;
      }
      .hero-title { font-size: 50px; font-weight: 800; margin-bottom: 10px; }
      .hero-desc { font-size: 18px; margin-bottom: 30px; }

      /* Program Cards */
      .program-card {
        background: white;
        padding: 25px;
        border-radius: 15px;
        text-align: center;
        transition: 0.3s;
        border: 2px solid #9CAB84;
        margin-bottom: 20px;
        cursor: pointer;
      }
      .program-card:hover { 
        transform: translateY(-5px); 
        box-shadow: 0 10px 20px rgba(0,0,0,0.1); 
        background-color: #F6F0D7;
      }

      /* Tool Cards */
      .card {
        background: white;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 6px 12px rgba(0,0,0,0.05);
        margin-bottom: 25px;
        border-left: 5px solid #89986D;
      }

      /* About App Specific Styles */
      .feature-list {
        background-color: #F6F0D7;
        padding: 20px;
        border-radius: 10px;
        margin: 15px 0;
        border-left: 5px solid #89986D;
      }
      
      .tech-stack {
        background-color: #C5D89D;
        padding: 20px;
        border-radius: 10px;
        margin: 15px 0;
      }
      
      .feature-item {
        margin: 10px 0;
        padding-left: 20px;
        position: relative;
      }
      
      .feature-item:before {
        content: '✓';
        position: absolute;
        left: 0;
        color: #2E7D32;
        font-weight: bold;
      }
      
      .tech-item {
        display: inline-block;
        background-color: #89986D;
        color: #000000;
        padding: 8px 15px;
        margin: 5px;
        border-radius: 20px;
        font-weight: bold;
      }

      /* Buttons */
      .btn-start { 
        background-color: #89986D; 
        color: #000000 !important; 
        padding: 15px 40px; 
        font-size: 20px; 
        font-weight: bold; 
        border-radius: 30px; 
        border: none;
      }
      
      .btn-back {
        background-color: #89986D;
        color: #000000 !important;
        padding: 10px 20px;
        margin-bottom: 20px;
        border-radius: 5px;
        border: none;
      }

      /* Chat Interface Styles */
      .chat-container {
        height: 400px;
        overflow-y: auto;
        border: 1px solid #9CAB84;
        border-radius: 10px;
        padding: 15px;
        background-color: #ffffff;
        margin-bottom: 20px;
      }
      
      .message {
        margin-bottom: 15px;
        padding: 10px 15px;
        border-radius: 15px;
        max-width: 80%;
        word-wrap: break-word;
      }
      
      .user-message {
        background-color: #C5D89D;
        margin-left: auto;
        text-align: right;
      }
      
      .assistant-message {
        background-color: #F6F0D7;
        margin-right: auto;
      }
      
      .api-key-input {
        background-color: #ffffff;
        padding: 15px;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      
      .api-status {
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
        font-weight: bold;
      }
      
      .api-valid {
        background-color: #4CAF50;
        color: #000000;
      }
      
      .api-invalid {
        background-color: #F94449;
        color: #000000;
      }
      
      .api-warning {
        background-color: #FFC107;
        color: #000000;
      }

      /* ===== BMI RESULT COLORS (OBVIOUS) ===== */
      .bmi-normal {
        background-color: #4CAF50 !important;   /* GREEN */
        color: #000000 !important;
        border-left: 8px solid #2E7D32 !important;
      }

      .bmi-over {
        background-color: #FFC107 !important;   /* ORANGE / YELLOW */
        color: #000000 !important;
        border-left: 8px solid #FF9800 !important;
      }

      .bmi-obese {
        background-color: #F94449 !important;   /* RED */
        color: #000000 !important;
        border-left: 8px solid #B71C1C !important;
      }

      .bmi-under {
        background-color: #2196F3 !important;   /* BLUE */
        color: #000000 !important;
        border-left: 8px solid #0D47A1 !important;
      }

      /* Reaction Test Styles */
      .reaction-success {
        background-color: #4CAF50 !important;
        color: #000000 !important;
        border-left: 8px solid #2E7D32 !important;
      }
      
      .reaction-fail {
        background-color: #F94449 !important;
        color: #000000 !important;
        border-left: 8px solid #B71C1C !important;
      }

    ")),
    tags$script(HTML("
      // Global variables for the timer
      var gameTimer = null;
      var gameStartTime = null;
      var gameRunning = false;
      
      // Function to start the timer
      function startGameTimer() {
        gameRunning = true;
        gameStartTime = Date.now();
        
        // Update the timer display every 10ms
        gameTimer = setInterval(function() {
          if (gameRunning) {
            var elapsed = (Date.now() - gameStartTime) / 1000;
            var timeDisplay = elapsed.toFixed(2);
            
            // Update the button text
            var button = document.getElementById('gameControl');
            if (button) {
              button.innerHTML = 'STOP (' + timeDisplay + 's)';
            }
            
            // Update the hidden input for Shiny
            Shiny.setInputValue('current_game_time', elapsed);
          }
        }, 10); // Update every 10ms for smooth display
      }
      
      // Function to stop the timer
      function stopGameTimer() {
        if (gameRunning) {
          gameRunning = false;
          clearInterval(gameTimer);
          var finalTime = (Date.now() - gameStartTime) / 1000;
          
          // Send the final time to Shiny
          Shiny.setInputValue('game_stopped', finalTime);
          
          // Reset button text
          var button = document.getElementById('gameControl');
          if (button) {
            button.innerHTML = 'START GAME';
          }
        }
      }
      
      // Handle button click
      $(document).on('click', '#gameControl', function() {
        if (!gameRunning) {
          // Start the game
          Shiny.setInputValue('game_started', Math.random());
          startGameTimer();
        } else {
          // Stop the game
          stopGameTimer();
        }
      });
      
      // Chat input focus and enter key
      $(document).ready(function() {
        $('#chatInput').focus();
        
        // Handle enter key in chat input
        $('#chatInput').keypress(function(e) {
          if (e.which == 13) {
            e.preventDefault();
            $('#sendChat').click();
          }
        });
      });
    "))
  ),

  # Main Navigation Container
  tabsetPanel(id = "main_navbar", type = "hidden",

    # --- PAGE 1: HOME ---
    tabPanel("home",
      div(class="hero-section",
        div(class="hero-title", "R Shiny Interactive Program"),
        div(class="hero-desc", "A comprehensive suite of interactive tools for data, health, and logic."),
        actionButton("btn_get_started", "Get Started", class="btn-start")
      )
    ),

    # --- PAGE 2: PROGRAM LIST ---
    tabPanel("programs",
      fluidRow(column(12, style="text-align:center; padding: 40px;", h2("Select a Program"))),
      fluidRow(
        column(4, div(class="program-card", actionLink("go_about", h3("📘 About App")))),
        column(4, div(class="program-card", actionLink("go_bmi", h3("⚖️ BMI Calculator")))),
        column(4, div(class="program-card", actionLink("go_summarizer", h3("🧠 Text Summarizer"))))
      ),
      fluidRow(
        column(4, div(class="program-card", actionLink("go_viz", h3("📊 Data Visualization")))),
        column(4, div(class="program-card", actionLink("go_react", h3("⏱️ Reaction Tester"))))
      ),
      div(style="text-align:center; margin-top:30px;", actionLink("back_to_home", "← Back to Home Screen"))
    ),

    # --- TOOL PAGES ---
    tabPanel("about_tool", actionButton("b1", "← Back", class="btn-back"), 
      div(class="card", 
        h3("📘 About This App"),
        p("This Shiny application demonstrates key programming language concepts such as reactive programming, event handling, data processing, and interactive visualization."),
        
        br(),
        
        # App Overview
        h4("🎯 App Overview"),
        div(class="feature-list",
          p("This interactive R Shiny application provides a suite of practical tools that demonstrate various programming and data science concepts:"),
          div(class="feature-item", "Multi-functional dashboard with 5 distinct tools"),
          div(class="feature-item", "Interactive user interface with smooth navigation"),
          div(class="feature-item", "Real-time data processing and visualization"),
          div(class="feature-item", "Educational focus on R programming concepts"),
          div(class="feature-item", "Responsive design with custom styling")
        ),
        
        br(),
        
        # Available Tools
        h4("🛠️ Available Tools"),
        div(class="feature-list",
          h5("1. ⚖️ BMI Calculator"),
          p("Calculate Body Mass Index with instant classification:"),
          div(class="feature-item", "Input height (cm) and weight (kg)"),
          div(class="feature-item", "Real-time BMI calculation"),
          div(class="feature-item", "Color-coded results (Underweight/Normal/Overweight/Obese)"),
          div(class="feature-item", "Visual feedback with emoji indicators"),
          
          br(),
          
          h5("2. 🧠 Text Summarizer"),
          p("Extract and summarize content from web URLs:"),
          div(class="feature-item", "Paste any website URL for analysis"),
          div(class="feature-item", "Local processing - no API keys required"),
          div(class="feature-item", "Advanced text extraction algorithms"),
          div(class="feature-item", "Key point identification and topic extraction"),
          div(class="feature-item", "Detailed analysis report"),
          
          br(),
          
          h5("3. 📊 Data Visualization"),
          p("Interactive chart generation with sample data:"),
          div(class="feature-item", "Multiple chart types (Line, Bar, Scatter)"),
          div(class="feature-item", "Customizable color schemes"),
          div(class="feature-item", "Adjustable point and line sizes"),
          div(class="feature-item", "Dynamic data refresh"),
          div(class="feature-item", "Professional styling options"),
          
          br(),
          
          h5("4. ⏱️ Reaction Tester"),
          p("Timing-based game to test reflexes:"),
          div(class="feature-item", "Set custom target times (1-30 seconds)"),
          div(class="feature-item", "Adjustable difficulty levels"),
          div(class="feature-item", "Real-time timer with millisecond precision"),
          div(class="feature-item", "Performance tracking and statistics"),
          div(class="feature-item", "Visual success/failure feedback")
        ),
        
        br(),
        
        # Technical Features
        h4("⚙️ Technical Features"),
        div(class="feature-list",
          div(class="feature-item", "Reactive Programming: Real-time UI updates based on user input"),
          div(class="feature-item", "Event Handling: Smooth navigation between tool sections"),
          div(class="feature-item", "Data Processing: Efficient data manipulation and transformation"),
          div(class="feature-item", "Web Scraping: URL content extraction using rvest package"),
          div(class="feature-item", "Text Analysis: Natural language processing for summarization"),
          div(class="feature-item", "Chart Generation: Dynamic visualization with ggplot2"),
          div(class="feature-item", "JavaScript Integration: Custom timer functionality"),
          div(class="feature-item", "Responsive Design: Mobile-friendly interface"),
          div(class="feature-item", "Custom CSS Styling: Professional and consistent look")
        ),
        
        br(),
        
        # Technology Stack
        h4("🚀 Technology Stack"),
        div(class="tech-stack",
          h5("Core R Packages Used:"),
          span(class="tech-item", "shiny"),
          span(class="tech-item", "dplyr"),
          span(class="tech-item", "ggplot2"),
          span(class="tech-item", "tidyr"),
          span(class="tech-item", "shinyjs"),
          span(class="tech-item", "httr"),
          span(class="tech-item", "rvest"),
          span(class="tech-item", "stringr"),
          span(class="tech-item", "xml2"),
          
          br(), br(),
          
          h5("Key Programming Concepts Demonstrated:"),
          span(class="tech-item", "Reactive Programming"),
          span(class="tech-item", "Event-Driven UI"),
          span(class="tech-item", "Data Visualization"),
          span(class="tech-item", "Web Scraping"),
          span(class="tech-item", "Text Processing"),
          span(class="tech-item", "Game Logic"),
          span(class="tech-item", "API-free AI")
        ),
        
        br(),
        
        # Educational Value
        h4("📚 Educational Value"),
        div(class="feature-list",
          p("This application serves as a comprehensive learning tool for:"),
          div(class="feature-item", "R Shiny framework and reactive programming"),
          div(class="feature-item", "Data visualization best practices"),
          div(class="feature-item", "Interactive web application development"),
          div(class="feature-item", "Real-world data processing techniques"),
          div(class="feature-item", "User experience design principles"),
          div(class="feature-item", "Problem-solving with programming")
        ),
        
        br(),
        
        # Developer Notes
        h4("👨‍💻 Developer Notes"),
        div(style = "background-color: #E8F5E9; padding: 15px; border-radius: 10px; border-left: 5px solid #4CAF50;",
          p("This application was developed to showcase:"),
          div(class="feature-item", "Practical implementation of R Shiny concepts"),
          div(class="feature-item", "Clean code organization and modular design"),
          div(class="feature-item", "User-friendly interface with intuitive navigation"),
          div(class="feature-item", "Real-world tool development approach"),
          div(class="feature-item", "Educational value through interactive features")
        ),
        
        br(),
        
        p(style = "text-align: center; font-style: italic; color: #666;",
          "🔧 Built with R Shiny | 📊 Interactive Data Tools | 🎯 Educational Focus"
        )
      )
    ),
    
    tabPanel("bmi_tool", actionButton("b2", "← Back", class="btn-back"),
      fluidRow(
        column(6, div(class="card", h4("⚖️ BMI Calculator"), numericInput("height", "Height (cm):", 170), numericInput("weight", "Weight (kg):", 65), actionButton("calcBMI", "Calculate BMI"))),
        column(6, uiOutput("bmiCard"))
      )
    ),

    tabPanel("viz_tool", actionButton("b4", "← Back", class="btn-back"),
      sidebarLayout(
        sidebarPanel(
          h4("📊 Visualization Controls"),
          selectInput("chart_type", "Chart Type:", 
                      choices = c("Line Chart" = "line", 
                                  "Bar Chart" = "bar", 
                                  "Scatter Plot" = "point")),
          
          # Simple Color Scheme dropdown
          selectInput("color_scheme", "Color Scheme:",
                      choices = c("Blue/Red" = "blue_red",
                                 "Green/Purple" = "green_purple", 
                                 "Orange/Teal" = "orange_teal",
                                 "Vibrant" = "vibrant")),
          
          sliderInput("point_size", "Point Size:", 1, 10, 3),
          sliderInput("line_size", "Line Size:", 0.5, 3, 1.5, step = 0.1),
          actionButton("updatePlot", "Refresh Data", class = "btn-primary")
        ),
        mainPanel(plotOutput("plotResult", height = "450px"))
      )
    ),

    tabPanel("react_tool", actionButton("b5", "← Back", class="btn-back"), 
      div(class="card", 
        h4("⏱️ Timing Challenge Game"),
        p("Set a target time and try to stop the timer exactly at that time!"),
        br(),
        
        # Game setup
        fluidRow(
          column(6,
            numericInput("target_time", "Target Time (seconds):", 
                        value = 5, min = 1, max = 30, step = 0.5,
                        width = "100%")
          ),
          column(6,
            selectInput("difficulty", "Difficulty Level:",
                       choices = c("Easy (±0.5s)" = 0.5,
                                  "Medium (±0.3s)" = 0.3,
                                  "Hard (±0.1s)" = 0.1),
                       selected = 0.5,
                       width = "100%")
          )
        ),
        br(),
        
        # Game controls and display
        div(style = "text-align: center;",
          # Game button with fixed ID
          tags$button(id = "gameControl", type = "button", class = "btn btn-default",
                     style = "width:100%; height:200px; font-size:36px; color:black; border-radius:12px; background-color:#89986D;",
                     "START GAME"),
          br(),
          h3(textOutput("gameResult")),
          br(),
          div(style = "font-size: 18px;",
            textOutput("gameStats")
          ),
          uiOutput("resultCard")
        ),
        br(),
        
        # Hidden inputs for JavaScript communication
        shinyjs::hidden(
          textInput("game_started", "game_started", ""),
          textInput("game_stopped", "game_stopped", ""),
          numericInput("current_game_time", "current_game_time", 0)
        ),
        
        # Game instructions
        div(class = "card", style = "background-color: #F6F0D7;",
          h5("🎮 How to Play:"),
          tags$ol(
            tags$li("Set your target time (e.g., 5 seconds)"),
            tags$li("Choose difficulty level (tolerance range)"),
            tags$li("Click START to begin the timer"),
            tags$li("Watch the timer grow on the button"),
            tags$li("Click STOP when you think it reaches your target time"),
            tags$li("Try to stop within the tolerance range!")
          )
        )
      )
    ),
    
    # --- TEXT SUMMARIZER TOOL PAGE ---
    tabPanel("summarizer_tool", actionButton("b3", "← Back", class="btn-back"),
      div(class="card",
        h4("🧠 URL Text Summarizer"),
        p("Summarize web page content from URLs using local processing only."),
        
        # Simple status indicator
        div(class="api-status",
            style = "background-color: #2196F3; color: #000000;",
            "📄 Local Summarizer Active: No API key needed, works offline!"),
        
        # Chat Interface
        div(class="chat-container", id="chatMessages",
          uiOutput("summaryHistory")
        ),
        
        # URL Input
        fluidRow(
          column(10,
            textInput("urlInput", NULL, 
                     placeholder = "Enter a URL to summarize (e.g., https://example.com)...",
                     width = "100%")
          ),
          column(2,
            actionButton("summarizeUrl", "Summarize", 
                        style = "width:100%; height:38px; background-color:#89986D; color:#000000;")
          )
        ),
        
        # Information
        div(style = "margin-top: 20px; font-size: 12px; color: #666;",
          p("💡 How to use:"),
          tags$ul(
            tags$li("Paste any website URL starting with http:// or https://"),
            tags$li("Click 'Summarize' to get a concise summary"),
            tags$li("All processing is done locally - no internet required for analysis"),
            tags$li("Only URL fetching requires internet connection")
          ),
          p("📚 Example URLs:"),
          tags$ul(
            tags$li("https://en.wikipedia.org/wiki/R_(programming_language)"),
            tags$li("https://www.bbc.com/news"),
            tags$li("https://www.nytimes.com")
          )
        )
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {

  # Navigation Logic
  observeEvent(input$btn_get_started, { updateTabsetPanel(session, "main_navbar", selected = "programs") })
  observeEvent(input$back_to_home, { updateTabsetPanel(session, "main_navbar", selected = "home") })
  
  observeEvent(input$go_about,      { updateTabsetPanel(session, "main_navbar", selected = "about_tool") })
  observeEvent(input$go_bmi,        { updateTabsetPanel(session, "main_navbar", selected = "bmi_tool") })
  observeEvent(input$go_summarizer, { updateTabsetPanel(session, "main_navbar", selected = "summarizer_tool") })
  observeEvent(input$go_viz,        { updateTabsetPanel(session, "main_navbar", selected = "viz_tool") })
  observeEvent(input$go_react,      { updateTabsetPanel(session, "main_navbar", selected = "react_tool") })

  lapply(1:5, function(i) { 
    observeEvent(input[[paste0("b", i)]], { 
      updateTabsetPanel(session, "main_navbar", selected = "programs") 
    }) 
  })

  # ===== BMI =====
  observeEvent(input$calcBMI, {
    bmi <- round(input$weight / ((input$height/100)^2), 2)

    if (bmi < 18.5) {
      cls <- "bmi-under"; msg <- "😕 Underweight"
    } else if (bmi < 25) {
      cls <- "bmi-normal"; msg <- "✅ Normal"
    } else if (bmi < 30) {
      cls <- "bmi-over"; msg <- "⚠️ Overweight"
    } else {
      cls <- "bmi-obese"; msg <- "❌ Obese"
    }

    output$bmiCard <- renderUI({
      div(
        class = paste("card", cls),
        h4("BMI Result"),
        h2(bmi),
        h4(msg)
      )
    })
  })

  # ===== VISUALIZATION =====
  create_sample_data <- function() { 
    data.frame(
      Month = factor(month.name, levels = month.name), 
      Cars = sample(100:300, 12), 
      Trucks = sample(50:200, 12)
    ) 
  }
  
  plotData <- reactiveVal(create_sample_data())
  observeEvent(input$updatePlot, { plotData(create_sample_data()) })
  
  output$plotResult <- renderPlot({
    df <- plotData()
    
    # Define color schemes
    colors <- switch(input$color_scheme,
                    "blue_red" = c("Cars" = "#1f77b4", "Trucks" = "#d62728"),
                    "green_purple" = c("Cars" = "#2ca02c", "Trucks" = "#9467bd"),
                    "orange_teal" = c("Cars" = "#ff7f0e", "Trucks" = "#17becf"),
                    "vibrant" = c("Cars" = "#e74c3c", "Trucks" = "#2ecc71"))
    
    if (input$chart_type == "line") {
      ggplot(df) + 
        geom_line(aes(Month, Cars, color = "Cars"), linewidth = input$line_size, group = 1) +
        geom_line(aes(Month, Trucks, color = "Trucks"), linewidth = input$line_size, linetype = "dashed", group = 1) +
        geom_point(aes(Month, Cars), size = input$point_size) + 
        geom_point(aes(Month, Trucks), size = input$point_size) +
        scale_color_manual(values = colors) + 
        theme_minimal() + 
        labs(title = "Monthly Vehicle Sales", 
             y = "Units Sold", 
             color = "Vehicle Type") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
      
    } else if (input$chart_type == "bar") {
      df_long <- pivot_longer(df, Cars:Trucks, names_to = "Vehicle", values_to = "Sales")
      ggplot(df_long, aes(Month, Sales, fill = Vehicle)) + 
        geom_col(position = "dodge") +
        scale_fill_manual(values = colors) + 
        theme_minimal() + 
        labs(title = "Monthly Vehicle Sales (Bar Chart)", 
             y = "Units Sold", 
             fill = "Vehicle Type") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
      
    } else {
      ggplot(df) + 
        geom_point(aes(Month, Cars, color = "Cars"), size = input$point_size) +
        geom_point(aes(Month, Trucks, color = "Trucks"), size = input$point_size) +
        scale_color_manual(values = colors) + 
        theme_minimal() + 
        labs(title = "Monthly Vehicle Sales (Scatter Plot)", 
             y = "Units Sold", 
             color = "Vehicle Type") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    }
  })

  # ===== TIMING CHALLENGE GAME =====
  gameState <- reactiveValues(
    running = FALSE,
    result = NULL,
    attempts = 0,
    successes = 0,
    bestScore = Inf,
    lastTime = 0
  )
  
  # Observe game started
  observeEvent(input$game_started, {
    if (input$game_started != "") {
      gameState$running <- TRUE
      gameState$result <- NULL
      gameState$lastTime <- 0
      
      # Update button color via JavaScript
      runjs("
        var button = document.getElementById('gameControl');
        if (button) {
          button.style.backgroundColor = '#ce2636';
        }
      ")
    }
  })
  
  # Observe game stopped
  observeEvent(input$game_stopped, {
    if (input$game_stopped > 0) {
      gameState$running <- FALSE
      final_time <- as.numeric(input$game_stopped)
      
      # Calculate the error
      target <- input$target_time
      error <- abs(final_time - target)
      tolerance <- as.numeric(input$difficulty)
      
      # Check if successful
      success <- error <= tolerance
      
      # Update game statistics
      gameState$attempts <- gameState$attempts + 1
      if (success) {
        gameState$successes <- gameState$successes + 1
      }
      
      # Update best score
      if (error < gameState$bestScore) {
        gameState$bestScore <- error
      }
      
      # Store result
      gameState$result <- list(
        success = success,
        time = final_time,
        error = error,
        target = target,
        tolerance = tolerance
      )
      
      # Reset button color via JavaScript
      runjs("
        var button = document.getElementById('gameControl');
        if (button) {
          button.style.backgroundColor = '#89986D';
          button.innerHTML = 'START GAME';
        }
      ")
    }
  })
  
  output$gameResult <- renderText({
    if (!is.null(gameState$result)) {
      if (gameState$result$success) {
        return(paste0("🎉 SUCCESS! You stopped at ", 
                     sprintf("%.2f", gameState$result$time), "s"))
      } else {
        return(paste0("❌ MISSED! You stopped at ", 
                     sprintf("%.2f", gameState$result$time), "s"))
      }
    }
    return("Set your target time and click START!")
  })
  
  output$gameStats <- renderText({
    if (gameState$attempts > 0) {
      accuracy <- if (gameState$attempts > 0) 
        round((gameState$successes / gameState$attempts) * 100, 1) 
      else 0
      
      best_score <- if (is.finite(gameState$bestScore)) 
        sprintf("%.3f", gameState$bestScore) 
      else "N/A"
      
      return(paste0("Attempts: ", gameState$attempts, 
                   " | Successes: ", gameState$successes,
                   " | Accuracy: ", accuracy, "%",
                   " | Best Error: ±", best_score, "s"))
    }
    return("No attempts yet. Start playing!")
  })
  
  output$resultCard <- renderUI({
    if (!is.null(gameState$result)) {
      if (gameState$result$success) {
        cls <- "reaction-success"
        icon <- "🎉"
        msg1 <- "PERFECT TIMING!"
        msg2 <- paste0("You stopped at ", sprintf("%.2f", gameState$result$time), "s")
        msg3 <- paste0("Target: ", gameState$result$target, "s | Error: ±", sprintf("%.3f", gameState$result$error), "s")
      } else {
        cls <- "reaction-fail"
        icon <- "❌"
        msg1 <- "TRY AGAIN!"
        msg2 <- paste0("You stopped at ", sprintf("%.2f", gameState$result$time), "s")
        msg3 <- paste0("Target: ", gameState$result$target, "s | Error: ±", sprintf("%.3f", gameState$result$error), "s")
      }
      
      div(
        class = paste("card", cls),
        style = "text-align: center; margin-top: 20px;",
        h3(paste(icon, msg1)),
        h4(msg2),
        p(msg3)
      )
    }
  })
  
  # Clear result when changing settings
  observeEvent(input$target_time, {
    gameState$result <- NULL
  })
  
  observeEvent(input$difficulty, {
    gameState$result <- NULL
  })
  
  # ===== URL TEXT SUMMARIZER =====
  
  # Reactive values for summarizer
  summaryState <- reactiveValues(
    history = list()
  )
  
  # Function to extract text from URL
  extract_text_from_url <- function(url) {
    tryCatch({
      # Add user agent to avoid blocking
      html <- read_html(url)
      
      # Extract text from paragraphs
      text <- html %>%
        html_nodes("p") %>%
        html_text() %>%
        paste(collapse = " ")
      
      # Extract title
      title <- html %>%
        html_nodes("title") %>%
        html_text() %>%
        paste(collapse = " | ")
      
      if (title == "") {
        title <- "Website Content"
      }
      
      # Clean up text
      text <- gsub("\\s+", " ", text)
      text <- str_trim(text)
      
      # Limit text length
      if (nchar(text) > 4000) {
        text <- substr(text, 1, 4000)
        text <- paste0(text, "... [text truncated]")
      }
      
      return(list(text = text, title = title))
    }, error = function(e) {
      return(list(text = paste("Error fetching URL:", e$message), title = "Error"))
    })
  }
  
  # Advanced local summarizer
  advanced_summarize <- function(text, title = "Content") {
    sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
    
    if (length(sentences) == 0) {
      return("No text found to summarize.")
    }
    
    # Enhanced stopwords list
    stopwords <- c("the", "is", "and", "a", "to", "of", "in", "that", "it", "on", 
                   "for", "with", "as", "are", "was", "were", "by", "an", "be", 
                   "this", "from", "or", "which", "you", "at", "but", "not", 
                   "have", "has", "had", "what", "when", "where", "how", "why",
                   "can", "could", "would", "should", "may", "might", "must",
                   "will", "shall", "they", "their", "them", "there", "these",
                   "those", "then", "than", "its", "our", "we", "us", "he", "she",
                   "him", "her", "his", "hers", "about", "all", "any", "both",
                   "each", "few", "more", "most", "other", "some", "such", "no",
                   "nor", "only", "own", "same", "so", "too", "very", "also")
    
    # Clean text for analysis
    clean_text <- tolower(gsub("[[:punct:]]", "", text))
    words <- unlist(strsplit(clean_text, "\\s+"))
    words <- words[!words %in% stopwords]
    words <- words[nchar(words) > 2]
    
    if (length(words) == 0) {
      # Return first few sentences
      return(paste("📋 **Summary of", title, ":**\n\n",
                   paste(sentences[1:min(4, length(sentences))], collapse = "\n\n")))
    }
    
    # Calculate word frequencies
    freq <- table(words)
    top_words <- names(sort(freq, decreasing = TRUE))[1:6]
    
    # Score sentences
    scores <- sapply(1:length(sentences), function(i) {
      s <- sentences[i]
      s_lower <- tolower(gsub("[[:punct:]]", "", s))
      s_words <- unlist(strsplit(s_lower, "\\s+"))
      
      # Multiple scoring factors
      keyword_score <- sum(freq[s_words], na.rm = TRUE)
      position_score <- ifelse(i <= 3, 3, ifelse(i <= 6, 2, ifelse(i <= 10, 1, 0)))
      length_score <- ifelse(nchar(s) > 30 & nchar(s) < 150, 2, ifelse(nchar(s) > 10, 1, 0))
      question_score <- ifelse(grepl("what|why|how|when|where|which|who", s_lower), 2, 0)
      important_words <- ifelse(grepl("important|key|main|primary|essential|critical", s_lower), 3, 0)
      
      return(keyword_score + position_score + length_score + question_score + important_words)
    })
    
    # Select diverse sentences
    top_indices <- c()
    if (length(sentences) > 0) top_indices <- c(1)  # Always include first
    
    sorted_indices <- order(scores, decreasing = TRUE)
    for (idx in sorted_indices) {
      if (!idx %in% top_indices && length(top_indices) < 5) {
        top_indices <- c(top_indices, idx)
      }
      if (length(top_indices) >= 5) break
    }
    
    top_indices <- sort(unique(top_indices))
    summary_sentences <- sentences[top_indices]
    
    # Create comprehensive summary
    summary <- paste(
      "📊 **URL Summary Report:**\n\n",
      "**Website:** ", title, "\n\n",
      "**Key Points:**\n",
      paste("•", summary_sentences, collapse = "\n"),
      "\n\n🔑 **Main Topics:** ",
      paste(top_words, collapse = ", "),
      "\n\n📈 **Analysis Details:**",
      paste0("\n- Sentences analyzed: ", length(sentences)),
      paste0("\n- Key points extracted: ", length(summary_sentences)),
      paste0("\n- Original text length: ", nchar(text), " characters"),
      "\n\n💡 **Note:** Generated using local summarization algorithm.",
      "\n⚡ **Processing:** 100% Local - No API Calls"
    )
    
    return(summary)
  }
  
  # Observe URL summarization
  observeEvent(input$summarizeUrl, {
    user_input <- trimws(input$urlInput)
    if (user_input == "") return()
    
    # Check if input is a URL
    is_url <- grepl("^https?://", user_input, ignore.case = TRUE)
    
    if (!is_url) {
      # Add error message
      summaryState$history <- append(summaryState$history, list(
        list(role = "error", content = "❌ Please enter a valid URL starting with http:// or https://")
      ))
      return()
    }
    
    # Add URL to history
    summaryState$history <- append(summaryState$history, list(
      list(role = "user", content = paste("🌐 URL:", user_input))
    ))
    
    # Clear input
    updateTextInput(session, "urlInput", value = "")
    
    # Show loading
    showModal(modalDialog("📡 Fetching URL content...", footer = NULL, easyClose = FALSE))
    
    # Extract text
    url_content <- extract_text_from_url(user_input)
    
    removeModal()
    
    if (grepl("^Error", url_content$text)) {
      summaryState$history <- append(summaryState$history, list(
        list(role = "assistant", content = paste("❌", url_content$text))
      ))
    } else {
      showModal(modalDialog("🔍 Analyzing and summarizing...", footer = NULL, easyClose = FALSE))
      
      # Local summarizer
      summary <- advanced_summarize(url_content$text, url_content$title)
      
      removeModal()
      
      # Add summary to history
      summaryState$history <- append(summaryState$history, list(
        list(role = "assistant", content = summary)
      ))
    }
    
    # Scroll to bottom
    runjs("
      var chatContainer = document.getElementById('chatMessages');
      if (chatContainer) {
        chatContainer.scrollTop = chatContainer.scrollHeight;
      }
    ")
  })
  
  # Render summary history
  output$summaryHistory <- renderUI({
    if (length(summaryState$history) == 0) {
      return(
        div(style = "text-align: center; color: #666; padding: 20px;",
            "🧠 URL Text Summarizer",
            tags$br(),
            tags$br(),
            "**Enter a URL to get started:**",
            tags$ul(style = "text-align: left; margin-top: 10px;",
              tags$li("Paste any website URL"),
              tags$li("Get instant AI-powered summary"),
              tags$li("100% local processing"),
              tags$li("No API keys required")
            ),
            tags$br(),
            "**Example URLs to try:**",
            tags$ul(style = "text-align: left; margin-top: 10px;",
              tags$li(tags$code("https://en.wikipedia.org/wiki/R_(programming_language)")),
              tags$li(tags$code("https://www.bbc.com/news")),
              tags$li(tags$code("https://www.nytimes.com"))
            ),
            tags$br(),
            div(style = "background-color: #F6F0D7; padding: 15px; border-radius: 10px;",
              "💡 The summarizer extracts main paragraphs and identifies key points using advanced text analysis algorithms."
            )
        )
      )
    }
    
    message_tags <- lapply(summaryState$history, function(msg) {
      if (msg$role == "user") {
        div(class = "message user-message",
            strong("URL: "),
            msg$content)
      } else if (msg$role == "assistant") {
        div(class = "message assistant-message",
            strong("Summary: "),
            HTML(gsub("\n", "<br>", msg$content)))
      } else if (msg$role == "error") {
        div(class = "message",
            style = "background-color: #FFEBEE; color: #C62828; font-style: italic; border-left: 3px solid #F44336;",
            HTML(gsub("\n", "<br>", msg$content)))
      }
    })
    
    tagList(message_tags)
  })
  
  # Auto-focus URL input
  observeEvent(input$go_summarizer, {
    runjs("setTimeout(function() { $('#urlInput').focus(); }, 100);")
  })
}

shinyApp(ui, server)