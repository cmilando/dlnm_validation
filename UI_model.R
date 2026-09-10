modelPanel <- tabPanel(
  "Model",
  
  helpText("The baseline case counts are modified according to the specified relative-risk surface to create an updated set of cases. A small amount of random variation is added to the simulated values. These updated cases are then analyzed using a quasi-Poisson regression model to estimate the temperature–risk relationship and assess how closely the fitted model recovers the specified relative-risk surface."),
  
  fluidRow(
    
    column(
      width = 4,
      
      div(
        class = "plot-container",
        
        textAreaInput(
          "model_argvar_text",
          "Exposure basis (ARGVAR)",
          value = "list(fun = 'ns', knots = c(50, 70))",
          rows = 3,
          width = "100%"
        ),
        
        helpText(
          "Example: list(fun = 'ns', knots = c(50, 70))"
        )
      )
    ),
    
    
    # -----------------------------------------------------
    # ARGLAG
    # -----------------------------------------------------
    
    column(
      width = 4,
      
      div(
        class = "plot-container",
        
        textAreaInput(
          "model_arglag_text",
          "Lag Basis (ARGLAG)",
          value = "list(fun = 'ns', knots = 2)",
          rows = 3,
          width = "100%"
        ),
        
        helpText(
          "Example: list(fun = 'ns', knots = 2)"
        )
      )
    ),
    
    column(
      width = 2,
      
      div(
        class = "plot-container",
        
        numericInput(
          "model_maxlag",
          "Maximum lag",
          value = 5,
          min = 0,
          max = 5,
          step = 1,
          width = "100%"
        )
        
        
      )
    ),
    
    column(
      width = 2,
      
      div(
        class = "plot-container",
        
        numericInput(
          "model_cen",
          "Centering T.",
          value = 45,
          step = 1,
          width = "100%"
        )
        
        
      )
    )
    
  ),
  
  tags$hr(),
  
  # =====================================================
  # Case data
  # =====================================================
  
  fluidRow(
    
    column(
      width = 12,
      
      div(
        class = "plot-container",
        
        h4(
          "Model results",
          style = "margin-top: 0; margin-bottom: 10px;"
        )
      )
    )
  ), 
  
  
  ####
  
  
  
  ####
  
  
  
  fluidRow(
    
    column(
      width = 6,
      
      fluidRow(
        
        column(
          width = 4,
          numericInput("case_year",
                       "Case Year",
                       value = 1980,
                       min = 1981, max = 1999, step = 1)
        ),
        
        column(
          width = 4,
          conditionalPanel(
            "input.baseline > 0",
            
            numericInput(
              "case_ymin",
              "Ymin",
              value = 0,
              width = "100%"
            )
          )
        ),
        
        column(
          width = 4,
          conditionalPanel(
            "input.baseline > 0",
            
            numericInput(
              "case_ymax",
              "Ymax",
              value = 0,
              width = "100%"
            )
          )
        )
        
      ),
      
      
      div(
        class = "plot-container",
        plotOutput(
          "case_timeseries",
          width = "100%",
          height = "300px"
        ),
        
        plotOutput(
          "temp_timeseries",
          width = "100%",
          height = "250px"
        )
      )
    ),
    column(
      width = 6,
      
      div(
        class = "plot-container",
        
        plotOutput(
          "case_scatter",
          width = "100%",
          height = "600px"
        )
      )
    )
    
  ),
  
  
  # =====================================================
  # Model outputs
  # =====================================================
  
  fluidRow(
    
    
    
    column(
      width = 6,
      
      div(
        class = "plot-container",
        
        h4(
          "Cumulative Exposure–Response",
          style = "margin-top: 0; margin-bottom: 10px;"
        ),
        
        plotOutput(
          "rr_overall",
          width = "100%",
          height = "350px"
        )
      )
    ),
    
    column(
      width = 6,
      
      div(
        class = "plot-container",
        
        h4(
          "Lag–Response",
          style = "margin-top: 0; margin-bottom: 10px;"
        ),
        
        plotOutput(
          "rr_lag",
          width = "100%",
          height = "350px"
        )
      )
    )
    
  ),
  
  
  # =====================================================
  # Model summary
  # =====================================================
  
  fluidRow(
    
    column(
      width = 4,
      
      div(
        class = "plot-container",
        
        h4(
          "Regression Coefficients",
          style = "margin-top: 0; margin-bottom: 10px;"
        ),
        
        tableOutput("model_coefficients")
      )
    ),
    
    column(
      width = 8,
      
      div(
        class = "plot-container",
        
        h4(
          "Model Summary",
          style = "margin-top: 0; margin-bottom: 10px;"
        ),
        
        verbatimTextOutput("model_summary")
      )
    )
    
  )
)