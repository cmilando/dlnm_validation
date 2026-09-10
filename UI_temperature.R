temperaturePanel <- tabPanel(
  "Temperature",
  
  helpText("This page shows a data generating mechanism for temperature
               exposure. Random uniform noise is added to this sine functino
               which is based on observed temperature data from Boston Logan
               airport."),
  
  fluidRow(
    
    column(
      width = 12,
      div(
        class = "plot-container",
        
        h4(
          "Use station data?",
          style = "margin-top: 0; margin-bottom: 15px;"
        ),
        
        checkboxInput("use_station_data", label = "Read file?"),
        
        helpText("Daily maximum temperature (°F), 1980–1999 at Boston Logan Airport, retrieved from https://psl.noaa.gov/data/timeseries/daily/"),
        tags$br(noWS=TRUE),
        helpText("File name: `getdailystat73.149.185.213.228.6.19.51`")
        
      )
    )
    
  ),
  
  fluidRow(
    
    column(
      width = 12,
      
      div(
        class = "plot-container",
        
        h4(
          "Simulation Parameters",
          style = "margin-top: 0; margin-bottom: 15px;"
        ),
        
        helpText("Parameters for working with simulated temperature data."),
        
        fluidRow(
          
          column(
            width = 2,
            numericInput(
              "amplitude_min",
              "Amplitude min.",
              value = 30,
              step = 5,
              width = "100%"
            )
          ),
          
          column(
            width = 2,
            numericInput(
              "amplitude_max",
              "Amplitude max.",
              value = 70,
              step = 5,
              width = "100%"
            )
          ),
          
          column(
            width = 2,
            numericInput(
              "temp_growth",
              "Annual growth",
              value = 0.01,
              step = 0.01,
              width = "100%"
            )
          ),
          
          column(
            width = 2,
            numericInput(
              "temp_noise",
              "Temp. noise",
              value = 35,
              min = 0,
              step = 5,
              width = "100%"
            )
          ),
          
          column(
            width = 2,
            numericInput(
              "temp_min",
              "T. lower bound",
              value = 0,
              step = 5,
              width = "100%"
            )
          ),
          
          column(
            width = 2,
            numericInput(
              "temp_max",
              "T. upper bound",
              value = 100,
              step = 5,
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
        
        shiny::hr(),
        
        h4(
          "Temperature Preview",
          style = "margin-top: 0; margin-bottom: 10px;"
        ),
        
        plotOutput(
          "temperature_plot",
          width = "100%",
          height = "300px"
        )
      )
    )
    
  ),
  
  fluidRow(
    column(
      width = 12,
      div(
        class = "plot-container",
        h4(
          "Temerature data summary",
          style = "margin-top: 0; margin-bottom: 10px;"
        ),
        verbatimTextOutput("temp_summary")
      )
    )
  )
  
)