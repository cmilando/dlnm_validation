populationPanel <- tabPanel(
  "Population",
  
  helpText("This page allows the user to define the baseline population,
               along with year trends, day of week trends."),
  
  fluidRow(
    
    # -----------------------------------------------------
    # Baseline cases
    # -----------------------------------------------------
    
    column(
      width = 2,
      
      div(
        class = "plot-container",
        
        h4(
          "Outcomes",
          style = "margin-top: 0; margin-bottom: 10px;"
        ),
        
        numericInput(
          "baseline",
          "Daily cases",
          value = 1000,
          min = 0,
          step = 10,
          width = "100%"
        )
        
      )
    ),
    
    column(
      width = 2,
      
      div(
        class = "plot-container",
        
        h4(
          " _ ",
          style = "margin-top: 0; margin-bottom: 10px;color: white;"
        ),
        
        numericInput(
          "rand",
          "Random",
          value = 0.03,
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
      width = 2,
      
      div(
        class = "plot-container",
        
        h4(
          " _ ",
          style = "margin-top: 0; margin-bottom: 10px;color: white;"
        ),
        
        numericInput(
          "year_growth",
          "Annual growth",
          value = 0.02,
          min = -1,
          max = 1,
          step = 0.01,
          width = "100%"
        )
        
      )
    ),
    
    column(
      width = 2,
      
      div(
        class = "plot-container",
        
        h4(
          " _ ",
          style = "margin-top: 0; margin-bottom: 10px;color: white;"
        ),
        
        numericInput(
          "dow_effect",
          "Day-of-week",
          value = 0.05,
          min = 0,
          max = 1,
          step = 0.01,
          width = "100%"
        )
      )
    ),
    
    column(
      width = 2,
      
      div(
        class = "plot-container",
        
        h4(
          " _ ",
          style = "margin-top: 0; margin-bottom: 10px;color: white;"
        ),
        
        numericInput(
          "season_effect",
          "Season",
          value = 0.05,
          min = -1,
          max = 1,
          step = 0.01,
          width = "100%"
        )
      )
    ),
    column(
      width = 2,
      
      div(
        class = "plot-container",
        
        h4(
          " _ ",
          style = "margin-top: 0; margin-bottom: 10px;color: white;"
        ),
        
        numericInput(
          "season_phase",
          "Season-phase",
          value = 7.5,
          step = 0.1,
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
              value = 1990,
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
      width = 6,
      
      div(
        class = "plot-container",
        
        h4(
          "Baseline Population Preview",
          style = "margin-top: 0; margin-bottom: 10px;"
        )
        
        
      )
    ), 
    
    column(
      width = 6,
      
      div(
        class = "plot-container",
        
        dateRangeInput(
          "xRange",
          "X Range",
          start = as.Date("1980-01-01"),
          end = as.Date("1999-12-31")
        )
        
        
      )
    ), 
    
  ),
  fluidRow(
    
    column(
      width = 12,
      
      div(
        class = "plot-container",
        
        plotOutput(
          "baseline_cases_plot",
          width = "100%",
          height = "300px"
        )
      )
    )
    
  )
)