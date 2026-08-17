library(shiny)
library(plotly)
library(splines)
library(purrr)
library(readr)
library(data.table)
library(ggcube)
library(ggplot2)

source("app_fcns.R")

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    
    .plot-container {
      padding: 8px;
      margin-bottom: 8px;
    }
    
    .plot-container h4 {
      margin-top: 0;
      margin-bottom: 5px;
    }
    
    .form-group {
      margin-bottom: 5px;
    }
    
    .control-label {
      margin-bottom: 2px;
    }
    
    .tab-content {
      padding-top: 5px;
    }
    
  "))
  ),
  
  tabsetPanel(
    
    # -----------------------------------------------------
    # Settings and help text
    # -----------------------------------------------------
    
    tabPanel(
      "Baseline Population",
      
      fluidRow(
        
        # -----------------------------------------------------
        # Baseline cases
        # -----------------------------------------------------
        
        column(
          width = 6,
          
          div(
            class = "plot-container",
            
            h4(
              "Baseline Cases",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            numericInput(
              "baseline",
              "Baseline daily cases",
              value = 1000,
              min = 0,
              step = 10,
              width = "100%"
            ),
            
            numericInput(
              "sd",
              "Random variation (SD)",
              value = 0.01,
              min = 0,
              step = 0.01,
              width = "100%"
            )
          )
        ),
        
        
        # -----------------------------------------------------
        # Trends
        # -----------------------------------------------------
        
        column(
          width = 6,
          
          div(
            class = "plot-container",
            
            h4(
              "Trends",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            numericInput(
              "year_growth",
              "Annual growth",
              value = 0.02,
              min = -1,
              max = 10,
              step = 0.01,
              width = "100%"
            ),
            
            numericInput(
              "dow_effect",
              "Day-of-week effect",
              value = 0.05,
              min = 0,
              max = 1,
              step = 0.01,
              width = "100%"
            )
          )
        )
        
      ),
      
      
      fluidRow(
        
        # -----------------------------------------------------
        # Location
        # -----------------------------------------------------
        
        column(
          width = 3,
          
          div(
            class = "plot-container",
            
            h4(
              "Location",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            textInput(
              "city",
              "City",
              value = "BOSTON",
              width = "100%"
            )
          )
          
        ),
        
        column(
          width = 3,
          
          div(
            class = "plot-container",
            
            h4(
              "Seed",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            numericInput(
              "seed",
              "Random Seed",
              value = 123,
              width = "100%"
            )
          )
          
        ),
        
        # -----------------------------------------------------
        # Time period
        # -----------------------------------------------------
        
        column(
          width = 6,
          
          div(
            class = "plot-container",
            
            h4(
              "Time Period",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            fluidRow(
              
              column(
                width = 6,
                
                numericInput(
                  "baseline_yr",
                  "Start year",
                  value = 1980,
                  min = 1980,
                  max = 1999,
                  step = 1,
                  width = "100%"
                )
              ),
              
              column(
                width = 6,
                
                numericInput(
                  "end_yr",
                  "End year",
                  value = 1999,
                  min = 1900,
                  max = 2100,
                  step = 1,
                  width = "100%"
                )
              )
              
            )
          )
        )
        
      ),
      
      
      # -------------------------------------------------------
      # Preview
      # -------------------------------------------------------
      
      fluidRow(
        
        column(
          width = 12,
          
          div(
            class = "plot-container",
            
            h4(
              "Baseline Population Preview",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            plotOutput(
              "baseline_cases_plot",
              width = "100%",
              height = "300px"
            )
          )
        )
        
      )
    ),
    
    # -----------------------------------------------------
    # Relative Risk
    # -----------------------------------------------------
    
    tabPanel(
      "Create RR surface",
      
      fluidRow(
        
        column(
          width = 4,
          div(
            class = "plot-container",
            rtPlotUI("plot1", "RR at lag=0",
                     "This graph determines the exposure and RR relationship
                     at the Initial lag value, i.e., lag = 0. This is the exposure
                     response curve for lag 0 only, and not taking
                     into account any other lags.")
          )
        ),
        
        column(
          width = 4,
          div(
            class = "plot-container",
            rtPlotUI("plot2", "RR at lag=MAX",
                     "This graph determines the exposure and RR relationship
                     at the MAXIMUM lag value. Note that this is *not* the 
                     same as the cumulative RR, rather this is the exposure
                     response curve for the maximum lag ONLY, and not taking
                     into account any other lags.")
          )
        ),
        
        column(
          width = 4,
          div(
            class = "plot-container",
            rtPlotUI("plot3", "Lag dimension",
                     "This graph determines the lag dimension, 
                     and the units are not important. This graph shows
                     how the surface behaves going along the lag dimension
                     and is rescaled for every starting and ending
                     point between RR at lag0 and lagMax for each 
                     exposure value.")
          )
        )
        
      ),
      
      combinedRRPlotUI(
        "combined1",
        "RR Surface - ORIG"
      )
      
    ),
    
    
    # -----------------------------------------------------
    # Additional tabs
    # -----------------------------------------------------
    
    tabPanel(
      "Population",
      
      h3("Results"),
      p("Results will go here.")
      
    )
    
  )
)
server <- function(input, output, session) {
  
  ## *******************************
  baseline_cases <- reactive({
    
    get_baseline_cases(
      baseline = input$baseline,
      sd = input$sd,
      year_growth = input$year_growth,
      dow_effect = input$dow_effect,
      city = input$city,
      baseline_yr = input$baseline_yr,
      end_yr = input$end_yr,
      seed = input$seed
    )
  })
  
  output$baseline_cases_plot <- renderPlot({
    
    df <- baseline_cases()
    
    plot(
      df$date,
      df$death,
      type = "l",
      xlab = "Date",
      ylab = "Cases"
    )
  })
  
  ## *******************************
  plot1 <- rtPlotServer(
    "plot1",
    x_init = c(1, 10, 20, 30),
    y_init = c(1, 1 ,1, 1.09),
    xlab = 'Temperature',
    ylab = 'RR',
    xmin = 1,
    xmax = 30,
    dx = 0.5,
    ymin = 0.9,
    ymax = 1.1
  )
  
  plot2 <- rtPlotServer(
    "plot2",
    x_init = c(1, 10, 20, 30),
    y_init = c(1, 1, 1, 1.02),
    xlab = 'Temperature',
    ylab = 'RR',
    xmin = 1,
    xmax = 30,
    dx = 0.5,
    ymin = 0.9,
    ymax = 1.1
  )
  
  plot3 <- rtPlotServer(
    "plot3",
    x_init = c(0, 3, 4, 5),
    y_init = c(1, 0.9, 0.75, 0.6),
    xlab = 'Lag',
    ylab = 'RR',
    xmin = 0,
    xmax = 5,
    dx = 0.5,
    ymin = 0.5,
    ymax = 1.1
  )
  
  RRmat_orig <- reactive({
    
    # first get the f_lag points
    x_vec = plot1$grid()$x
    nx = length(x_vec)
    f_lag_base = plot3$grid()$y
    f_exp_max = plot1$grid()$y
    f_exp_init = plot2$grid()$y

    RR <- function(i)  {
      local_rescale(f_lag_base, 
                    f_exp_max[i],
                    f_exp_init[i])
    }
    
    RR_mat <- do.call(cbind, lapply(1:nx, RR))
    
    RR_mat
    
  })
  
  ##
  combinedRRPlotServer(
    "combined1",
    RRmat_reactive = RRmat_orig,
    x = reactive(plot1$grid()$x),
    l = reactive(plot3$grid()$x)
  )
  
}

shinyApp(
  ui = ui,
  server = server
)