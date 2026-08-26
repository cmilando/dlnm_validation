trueRRPanel <- tabPanel(
  "True RR",
  
  helpText("This page converts the user-defined surface from the 
               previous page into the true data generating mechansim.
               The boxes below are used to create the crossbasis object,
               and then the beta coefficients are estimated using matrix
               inversion of the created RR surface. Then on this page, these
               beta coefficients are used to estimate the true surface. This
               process is necessary because there is no guarantee that the
               user-generated surface on the previous page will be able
               to be matched by any set of splines. Following the result of
               this page we have a surface and beta coefficients that we 
               can use to evaluate DLNM performance under a variety of 
               scenarios"),
  
  fluidRow(
    
    column(
      width = 12,
      
      div(
        class = "plot-container",
        h4(
          "Parameters for True Underlying RR and Data Generation",
          style = "margin-top: 0; margin-bottom: 10px;"
        )
      ))),
  
  # -----------------------------------------------------
  # ARGVAR
  # -----------------------------------------------------
  fluidRow(
    column(
      width = 4,
      
      div(
        class = "plot-container",
        
        textAreaInput(
          "true_argvar_text",
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
          "true_arglag_text",
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
          "true_maxlag",
          "Max. lag",
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
          "true_cen",
          "Centering T.",
          value = 45,
          step = 1,
          width = "100%"
        )
        
        
      )
    )
    
  ),
  
  # -------------------------------------------------------
  # Results
  # -------------------------------------------------------
  shiny::hr(),
  
  combinedRRPlotUI(
    "true_rr_surface",
    "True RR Surface",
    pOther = plotOutput("trueRRCumulative",               
                        width = "95%",
                        height = "300px"),
    hOther = helpText("This plots the cumulative RR, i.e.,",
                      "the sum of each of the risk curves from
                        lag 0 to maxlag. This is the same as the output",
                      "from plot.crosspred(cp, 'overall')")
    
  ),
  
  shiny::hr(),
  
  fluidRow(
    
    column(width = 4,
           h4(
             "Updated outcome timeseries",
             style = "margin-top: 0; margin-bottom: 10px;"
           )
    ),
    column(width = 2,
           p("Poisson draw"),
           checkboxInput(
             "pois_draw",
             "",
             value = T
           )
    ),
    
    column(width = 2,
           numericInput(
             "full_case_ymin",
             "Ymin",
             value = 0,
             width = "100%"
           )
    ),
    column(width = 2,
           numericInput(
             "full_case_ymax",
             "Ymax",
             value = 0,
             width = "100%")
    )
  ),
  fluidRow(
    column(
      width = 12,
      plotOutput(
        "full_case_timeseries",
        width = "100%",
        height = "300px"
      )
    )
  )
)