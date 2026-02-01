# ===============================
# R Shiny Multi-Function Demo App
# ===============================

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinyjs)
library(plotly)
library(readxl)

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
                       fluidRow(column(12, style="text-align:center; padding: 40px;", h2("Welcome to R with Shiny Application"))),
                       fluidRow(
                         column(4, div(class="program-card", actionLink("go_about", h3("📘 About App")))),
                         column(4, div(class="program-card", actionLink("go_bmi", h3("⚖️ BMI Calculator")))),
                         column(4, div(class="program-card", actionLink("go_viz", h3("📊 Data Visualization"))))
                       ),
                       fluidRow(
                         column(4, offset=4, div(class="program-card", actionLink("go_react", h3("⏱️ Reaction Tester"))))
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
                               div(class="feature-item", "Multi-functional dashboard with 3 distinct tools"),
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
                               
                               h5("2. 📊 Data Visualization"),
                               p("Power BI–style interactive charts from your own data:"),
                               div(class="feature-item", "Upload CSV or XLSX files from your device"),
                               div(class="feature-item", "Chart types: Line, Bar, Stacked Bar, Area, Scatter, Pie/Donut"),
                               div(class="feature-item", "Choose X and Y columns dynamically"),
                               div(class="feature-item", "Interactive plots: hover, zoom, pan, toggle series"),
                               div(class="feature-item", "Color schemes and styling options"),
                               
                               br(),
                               
                               h5("3. ⏱️ Reaction Tester"),
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
                               span(class="tech-item", "plotly"),
                               span(class="tech-item", "readxl"),
                               span(class="tech-item", "tidyr"),
                               span(class="tech-item", "shinyjs"),
                               
                               br(), br(),
                               
                               h5("Key Programming Concepts Demonstrated:"),
                               span(class="tech-item", "Reactive Programming"),
                               span(class="tech-item", "Event-Driven UI"),
                               span(class="tech-item", "Data Visualization"),
                               span(class="tech-item", "Game Logic"),
                               span(class="tech-item", "JavaScript Integration")
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
                       fluidRow(
                         column(12, div(class = "card", style = "margin-bottom: 15px;",
                           h4("📊 Data Visualization — Power BI Style"),
                           p("Upload a CSV or XLSX file from your device, then choose columns and chart type. Charts are interactive: hover, zoom, pan, and toggle series.")
                         ))
                       ),
                       sidebarLayout(
                         sidebarPanel(
                           width = 4,
                           h4("📁 Data Source"),
                           fileInput("viz_file", "Upload CSV or XLSX",
                                     accept = c(".csv", ".xlsx", ".xls"),
                                     buttonLabel = "Browse...",
                                     placeholder = "No file chosen"),
                           checkboxInput("viz_use_sample", "Use sample data (no file)", value = TRUE),
                           hr(),
                           h4("📐 Chart Settings"),
                           selectInput("viz_x", "X / Category", choices = "—", selected = NULL),
                           selectInput("viz_y", "Y / Value", choices = c("(none)", "—"), selected = "(none)"),
                           selectInput("viz_chart_type", "Chart Type:",
                                       choices = c("Line Chart" = "line",
                                                   "Bar Chart" = "bar",
                                                   "Stacked Bar" = "stacked_bar",
                                                   "Area Chart" = "area",
                                                   "Scatter Plot" = "scatter",
                                                   "Pie / Donut" = "pie")),
                           selectInput("viz_color_scheme", "Color Scheme:",
                                       choices = c("Power BI Blue" = "powerbi",
                                                   "Blue" = "blue_red",
                                                   "Green" = "green_purple",
                                                   "Orange" = "orange_teal",
                                                   "Vibrant" = "vibrant")),
                           sliderInput("viz_point_size", "Point Size:", 1, 12, 5),
                           sliderInput("viz_line_size", "Line Size:", 0.5, 4, 1.5, step = 0.1),
                           actionButton("viz_download_png", "Download as PNG", class = "btn-primary")
                         ),
                         mainPanel(
                           width = 8,
                           uiOutput("viz_data_preview_ui"),
                           plotlyOutput("viz_plot", height = "520px")
                         )
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
  
  # ===== VISUALIZATION (File upload + Power BI–style charts) =====
  create_sample_data <- function() {
    set.seed(42)
    data.frame(
      Month = factor(month.name, levels = month.name),
      Cars = sample(100:300, 12),
      Trucks = sample(50:200, 12)
    )
  }

  observeEvent(input$viz_file, {
    if (!is.null(input$viz_file) && nrow(input$viz_file) > 0)
      updateCheckboxInput(session, "viz_use_sample", value = FALSE)
  })

  viz_raw <- reactive({
    use_sample <- isTRUE(input$viz_use_sample)
    f <- input$viz_file
    if (use_sample) return(create_sample_data())
    if (is.null(f) || !nzchar(f$datapath[1])) return(create_sample_data())
    path <- f$datapath[1]
    ext <- tolower(tools::file_ext(f$name[1]))
    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        df <- as.data.frame(read_excel(path, sheet = 1))
      } else {
        df <- as.data.frame(read.csv(path, header = TRUE, stringsAsFactors = FALSE,
                                     check.names = FALSE, na.strings = c("", "NA")))
      }
      if (is.null(df) || nrow(df) == 0) return(create_sample_data())
      df
    }, error = function(e) {
      create_sample_data()
    })
  })

  viz_cols <- reactive(names(viz_raw()))

  viz_numeric <- reactive({
    nms <- viz_cols()
    if (length(nms) == 0) return(character(0))
    ok <- vapply(viz_raw()[nms], function(x) is.numeric(x) || is.integer(x), logical(1))
    nms[ok]
  })

  observeEvent(viz_raw(), {
    df <- viz_raw()
    cols <- names(df)
    nums <- viz_numeric()
    if (length(cols) == 0) return()
    updateSelectInput(session, "viz_x", choices = cols, selected = cols[1])
    updateSelectInput(session, "viz_y", choices = c("(none)", cols),
                     selected = if (length(nums) > 0) nums[1] else "(none)")
  }, ignoreInit = FALSE)

  output$viz_data_preview_ui <- renderUI({
    df <- viz_raw()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    tbl <- head(df, 6)
    hd <- tags$tr(lapply(names(tbl), function(n) tags$th(style = "text-align:left; padding:4px 8px;", n)))
    bd <- lapply(seq_len(nrow(tbl)), function(i) {
      tags$tr(lapply(unlist(tbl[i, ], use.names = FALSE), function(cell) tags$td(style = "padding:4px 8px;", as.character(cell))))
    })
    div(
      class = "card",
      style = "margin-bottom: 15px; padding: 12px;",
      h5("📋 Data preview"),
      div(style = "overflow-x: auto; max-height: 140px; font-size: 13px;",
          tags$table(class = "table", style = "width:100%; border-collapse: collapse;",
                     tags$thead(hd), tags$tbody(bd)))
    )
  })

  viz_plot_df <- reactive({
    df <- viz_raw()
    x <- input$viz_x
    y <- input$viz_y
    if (is.null(df) || nrow(df) == 0) return(NULL)
    cols <- names(df)
    if (length(cols) == 0) return(NULL)
    if (is.null(x) || !x %in% cols) x <- cols[1]
    use_y <- !is.null(y) && y != "(none)" && y %in% cols
    list(df = df, x = x, y = y, use_y = use_y)
  })

  viz_colors <- reactive({
    sch <- input$viz_color_scheme
    switch(
      sch,
      "powerbi" = c("#118DFF", "#12239E", "#E66C37", "#6B007B", "#E044A7", "#744EC2", "#D9B300", "#1B587C"),
      "blue_red" = c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e"),
      "green_purple" = c("#2ca02c", "#9467bd", "#17becf", "#e74c3c"),
      "orange_teal" = c("#ff7f0e", "#17becf", "#2ca02c", "#d62728"),
      "vibrant" = c("#e74c3c", "#2ecc71", "#3498db", "#f39c12", "#9b59b6")
    )
  })

  viz_plot_obj <- reactive({
    L <- viz_plot_df()
    if (is.null(L)) return(plotly_empty())
    df <- L$df
    x <- L$x
    y <- L$y
    use_y <- L$use_y
    chart <- input$viz_chart_type
    cols <- viz_colors()
    pt <- input$viz_point_size
    ln <- input$viz_line_size

    # For pie, use first numeric as values, x as labels; if no numeric, count by x
    if (chart == "pie") {
      if (use_y && y %in% names(df)) {
        agg <- aggregate(as.formula(paste0("`", y, "` ~ `", x, "`")), data = df, FUN = sum)
        lbl <- agg[[1]]
        val <- agg[[2]]
      } else {
        agg <- as.data.frame(table(df[[x]]))
        lbl <- agg[[1]]
        val <- agg$Freq
      }
      pl <- plot_ly(labels = lbl, values = val, type = "pie", hole = 0.5,
                    textinfo = "label+percent", hovertemplate = "%{label}: %{value} (%{percent})<extra></extra>",
                    marker = list(colors = cols[seq_along(lbl)]))
      pl <- pl %>% layout(title = list(text = "Distribution", font = list(size = 16)),
                          showlegend = TRUE, legend = list(orientation = "h", y = -0.15),
                          margin = list(t = 50, b = 50), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
      return(pl %>%
        config(displayModeBar = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d")) %>%
        htmlwidgets::onRender("
          function(el, x) {
            var gd = el.querySelector('.js-plotly-plot') || el;
            if (gd) window.__viz_plot_gd = gd;
          }
        "))
    }

    # Build plot data: single Y or pivot all numerics
    if (use_y) {
      df_plot <- df
      xcol <- x
      ycol <- y
      gcol <- NULL
    } else {
      num_cols <- viz_numeric()
      num_cols <- setdiff(num_cols, x)
      if (length(num_cols) == 0) return(plotly_empty())
      df_long <- tidyr::pivot_longer(df, dplyr::all_of(num_cols), names_to = ".series", values_to = ".value")
      df_plot <- df_long
      xcol <- x
      ycol <- ".value"
      gcol <- ".series"
    }

    ax_opts <- list(zeroline = TRUE, gridcolor = "rgba(0,0,0,0.06)", tickfont = list(size = 11))
    tit <- if (use_y) paste0(ycol, " vs ", xcol) else paste0("Values by ", xcol)

    if (chart == "line") {
      if (!is.null(gcol) && gcol %in% names(df_plot)) {
        pl <- plot_ly(df_plot, x = ~.data[[xcol]], y = ~.data[[ycol]], color = ~.data[[gcol]],
                     type = "scatter", mode = "lines+markers",
                     line = list(width = ln), marker = list(size = pt),
                     colors = cols)
      } else {
        pl <- plot_ly(df_plot, x = ~.data[[xcol]], y = ~.data[[ycol]],
                     type = "scatter", mode = "lines+markers",
                     line = list(width = ln, color = cols[1]),
                     marker = list(size = pt, color = cols[1]))
      }
      pl <- pl %>% layout(xaxis = c(ax_opts, list(title = xcol)), yaxis = c(ax_opts, list(title = ycol)),
                          title = list(text = tit, font = list(size = 16)),
                          legend = list(orientation = "h", y = -0.12),
                          margin = list(t = 50, b = 80),
                          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
    } else if (chart == "area") {
      if (!is.null(gcol) && gcol %in% names(df_plot)) {
        pl <- plot_ly(df_plot, x = ~.data[[xcol]], y = ~.data[[ycol]], color = ~.data[[gcol]],
                     type = "scatter", mode = "lines", fill = "tozeroy",
                     line = list(width = ln), colors = cols)
      } else {
        pl <- plot_ly(df_plot, x = ~.data[[xcol]], y = ~.data[[ycol]],
                     type = "scatter", mode = "lines", fill = "tozeroy",
                     line = list(width = ln, color = cols[1]))
      }
      pl <- pl %>% layout(xaxis = c(ax_opts, list(title = xcol)), yaxis = c(ax_opts, list(title = ycol)),
                          title = list(text = tit, font = list(size = 16)),
                          legend = list(orientation = "h", y = -0.12),
                          margin = list(t = 50, b = 80),
                          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
    } else if (chart %in% c("bar", "stacked_bar")) {
      pos <- if (chart == "stacked_bar") "stack" else "group"
      if (!is.null(gcol) && gcol %in% names(df_plot)) {
        pl <- plot_ly(df_plot, x = ~.data[[xcol]], y = ~.data[[ycol]], color = ~.data[[gcol]],
                     type = "bar", colors = cols) %>%
          layout(barmode = pos)
      } else {
        pl <- plot_ly(df_plot, x = ~.data[[xcol]], y = ~.data[[ycol]],
                     type = "bar", marker = list(color = cols[1]))
      }
      pl <- pl %>% layout(xaxis = c(ax_opts, list(title = xcol)), yaxis = c(ax_opts, list(title = ycol)),
                          title = list(text = tit, font = list(size = 16)),
                          legend = list(orientation = "h", y = -0.12),
                          margin = list(t = 50, b = 80),
                          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
    } else {
      if (!is.null(gcol) && gcol %in% names(df_plot)) {
        pl <- plot_ly(df_plot, x = ~.data[[xcol]], y = ~.data[[ycol]], color = ~.data[[gcol]],
                     type = "scatter", mode = "markers", marker = list(size = pt), colors = cols)
      } else {
        pl <- plot_ly(df_plot, x = ~.data[[xcol]], y = ~.data[[ycol]],
                     type = "scatter", mode = "markers",
                     marker = list(size = pt, color = cols[1]))
      }
      pl <- pl %>% layout(xaxis = c(ax_opts, list(title = xcol)), yaxis = c(ax_opts, list(title = ycol)),
                          title = list(text = tit, font = list(size = 16)),
                          legend = list(orientation = "h", y = -0.12),
                          margin = list(t = 50, b = 80),
                          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
    }
    pl %>%
      config(displayModeBar = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d")) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var gd = el.querySelector('.js-plotly-plot') || el;
          if (gd) window.__viz_plot_gd = gd;
        }
      ")
  })

  output$viz_plot <- renderPlotly({ viz_plot_obj() })

  observeEvent(input$viz_download_png, {
    runjs("
      (function() {
        var gd = window.__viz_plot_gd;
        if (!gd || typeof Plotly === 'undefined') return;
        setTimeout(function() {
          Plotly.downloadImage(gd, { format: 'png', width: 900, height: 520, filename: 'chart' });
        }, 150);
      })();
    ")
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
}

shinyApp(ui, server)