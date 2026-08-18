library(shiny)
library(plotly)
library(splines)
library(purrr)
library(readr)
library(data.table)
library(ggcube)
library(ggplot2)
library(bslib)
library(shinyjs)

source("app_fcns.R")

ui <- fluidPage(
  
  useShinyjs(),
  
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
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  div(
    style = "max-width: 1000px; margin: auto;",
  
  tabsetPanel(
    
    tabPanel(
      "Info",
      
      fluidRow(
        
        column(
          width = 10,
          offset = 1,
          
          div(
            class = "plot-container",
            style = "
          padding: 30px 40px;
          background-color: #f9f9f9;
          border: 0.5px solid #ccc;
          border-radius: 4px;
          margin-top: 20px;",
            
            # -------------------------------------------------
            # Title
            # -------------------------------------------------
            
            h2(
              "Assessing Validity of Distributed Lag Non-Linear Models 
              Applied in Environmental Epidemiology",
              style = "
            text-align: center;
            line-height: 1.3;
            margin-top: 0;
            margin-bottom: 20px;
          "
            ),
            
            # -------------------------------------------------
            # Authors
            # -------------------------------------------------
            
            p(
              "Chad W. Milando, Quinn H. Adams, Gregory A. Wellenius",
              style = "
            text-align: center;
            font-size: 16px;
            margin-bottom: 5px;
          "
            ),
            
            p(
              "Boston University School of Public Health",
              style = "
            text-align: center;
            font-size: 15px;
            color: #666;
            margin-bottom: 30px;
          "
            ),
            
            hr(),
            
            # -------------------------------------------------
            # Abstract
            # -------------------------------------------------
            
            h3(
              "Abstract",
              style = "
            margin-top: 25px;
            margin-bottom: 15px;
          "
            ),
            
            p(
              paste0(
                "Distributed Lag Non-Linear ",
                "Models, or DLNMs, permit simultaneous accounting of ",
                "exposure magnitude and timing in the estimation of outcome risk. ",
                "Presently DLNMs are ubiquitous in analyses of environmental ",
                "epidemiology, due to the above capabilities and the ease of ",
                "implementation via the R package `dlnm`.\n",
                "However, despite more than a decade of use, there has been ",
                "no validation study of DLNMs against simulated data where the ",
                "true exposure and outcome relationships are known. Such a ",
                "validation is essential to ensure that users of DLNM understand ",
                "the model’s strengths and limitations. We prepared simulated ",
                "datasets to explore challenges common in DLNM modeling."
              ),
              style = "
            font-size: 15px;
            line-height: 1.7;
            text-align: justify;
          "
            ),
            
            # -------------------------------------------------
            # Funding attribution
            # -------------------------------------------------
            
            h5(
              "Funding attribution",
              style = "margin-top: 30px;margin-bottom: 15px;"
            ),
            
            p(
              paste0(
                "Support for this project comes from the Massachusetts Municipal ",
                "Vulnerability Preparedness (MVP) program, and the Wellcome ",
                "Foundation for the Community Adaptations for City Heat Project ",
                "(CATCH) at Boston University (Climate Impact Award ",
                "311886/Z/24/Z)."
              ),
              style = "
                        font-size: 15px;
                        line-height: 1.7;
                        text-align: justify;
                      "
            )
            
          )
        )
        
      )
    ),
    
    # -----------------------------------------------------
    # Settings and help text
    # -----------------------------------------------------
    
    tabPanel(
      "Population",
      
      helpText("This page allows the user to define the baseline population,
               along with year trends, day of week trends."),
      
      fluidRow(
        
        # -----------------------------------------------------
        # Baseline cases
        # -----------------------------------------------------
        
        column(
          width = 3,
          
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
            
          )
        ),
        
        column(
          width = 3,
          
          div(
            class = "plot-container",
            
            h4(
              " _ ",
              style = "margin-top: 0; margin-bottom: 10px;color: white;"
            ),
            
            numericInput(
              "sd",
              "Random variation (SD)",
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
          width = 3,
          
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
              max = 1,
              step = 0.01,
              width = "100%"
            ),
            
          )
        ),
        
        column(
          width = 3,
          
          div(
            class = "plot-container",
            
            h4(
              " _ ",
              style = "margin-top: 0; margin-bottom: 10px;color: white;"
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
    
    tabPanel(
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
            
            fluidRow(
              
              column(
                width = 2,
                numericInput(
                  "temp_min",
                  "Minimum temp.",
                  value = 30,
                  step = 0.1,
                  width = "100%"
                )
              ),
              
              column(
                width = 2,
                numericInput(
                  "temp_max",
                  "Maximum temp.",
                  value = 70,
                  step = 0.1,
                  width = "100%"
                )
              ),
              
              column(
                width = 2,
                numericInput(
                  "temp_phase",
                  "Phase shift",
                  value = 4.44,
                  step = 0.01,
                  width = "100%"
                )
              ),
              
              column(
                width = 2,
                numericInput(
                  "temp_growth",
                  "Annual growth",
                  value = 0.01,
                  step = 0.001,
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
      
    ),
    
    # -----------------------------------------------------
    # Relative Risk
    # -----------------------------------------------------
    
    tabPanel(
      "RR surface",
      
      helpText("This page allows the user to defined the RR surface visually.
               The spline degree can change how linear the surfaces interact.
               The lag graph determines how the surface behaves in the lag 
               dimension. This is not fully bulletproof,
               there are likely some edge cases that will break how this 
               works."),
      
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
      
      ##
      combinedRRPlotUI(
        "combined1",
        "RR Surface - ORIG"
      )
      
      ##
      
    ),
    
    tabPanel(
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
        
        # -----------------------------------------------------
        # ARGVAR
        # -----------------------------------------------------
        
        column(
          width = 6,
          
          div(
            class = "plot-container",
            
            h4(
              "Exposure Basis (ARGVAR)",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            textAreaInput(
              "argvar_text",
              "ARGVAR",
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
          width = 6,
          
          div(
            class = "plot-container",
            
            h4(
              "Lag Basis (ARGLAG)",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            textAreaInput(
              "arglag_text",
              "ARGLAG",
              value = "list(fun = 'ns', knots = 2)",
              rows = 3,
              width = "100%"
            ),
            
            helpText(
              "Example: list(fun = 'ns', knots = 2)"
            )
          )
        )
        
      ),
      
      
      # -------------------------------------------------------
      # Other settings
      # -------------------------------------------------------
      
      fluidRow(
        
        column(
          width = 3,
          
          div(
            class = "plot-container",
            
            numericInput(
              "maxlag",
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
          width = 3,
          
          div(
            class = "plot-container",
            
            numericInput(
              "cen",
              "Centering temperature",
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
      
      combinedRRPlotUI(
        "true_rr_surface",
        "True RR Surface"
      ),
      
      h4(
        "True Cumulative RR",
        style = "margin-top: 0; margin-bottom: 10px;"
      ),
      
      plotOutput("trueRRCumulative",               
                 width = "50%",
                 height = "300px")
    ),
    
    
    # -----------------------------------------------------
    # Additional tabs
    # -----------------------------------------------------
    
    tabPanel(
      "Model",
      
      helpText("The baseline case counts are modified according to the specified relative-risk surface to create an updated set of cases. A small amount of random variation is added to the simulated values. These updated cases are then analyzed using a quasi-Poisson regression model to estimate the temperature–risk relationship and assess how closely the fitted model recovers the specified relative-risk surface."),
      
      fluidRow(
        
        column(
          width = 2,
          numericInput("case_year",
                       "Case Year",
                       value = 1980,
                       min = 1981, max = 1999, step = 1)
        ),
        
        column(
          width = 3,
          numericInput("model_error_sd",
                       "Model error",
                       value = 0.1, min = 0.0000001, step = 0.01)
        ),
        
        column(
          width = 3,
          p("Take Poisson draw"),
          checkboxInput(
            "pois_draw",
            "",
            value = T
          )

        ),
        
        column(
          width = 2,
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
          width = 2,
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
      
      # =====================================================
      # Case data
      # =====================================================
      
      fluidRow(
        
        column(
          width = 12,
          
          div(
            class = "plot-container",
            
            h4(
              "Updated Case Data",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
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
    ),
    
    tabPanel(
      "Download",
      
      fluidRow(
        
        column(
          width = 8,
          offset = 2,
          
          div(
            class = "plot-container",
            
            h4(
              "Temperature and Health Data",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            p(
              "Download the temperature and health data joined by date, city, and year."
            ),
            
            br(),
            
            downloadButton(
              "download_csv",
              "Download CSV",
              class = "btn-primary"
            )
            
          )
        )
        
      )
    )
    
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
  
  observeEvent(input$use_station_data, {
    
    if (input$use_station_data) {
      
      shinyjs::disable("temp_min")
      shinyjs::disable("temp_max")
      shinyjs::disable("temp_phase")
      shinyjs::disable("temp_growth")
      shinyjs::disable("temp_noise")
      
    } else {
      
      shinyjs::enable("temp_min")
      shinyjs::enable("temp_max")
      shinyjs::enable("temp_phase")
      shinyjs::enable("temp_growth")
      shinyjs::enable("temp_noise")
      
    }
  })
  
  temp_data <- reactive({
    
    if(input$use_station_data) {
      
      get_temp_data(input)
      
    } else {
    
      # -------------------------------------------------------
      # Get dates from baseline population
      # -------------------------------------------------------
      
      baseline <- baseline_cases()
      
      dates <- baseline$date
      
      # -------------------------------------------------------
      # Daily index
      # -------------------------------------------------------
      
      idx <- seq_along(dates)
      
      # -------------------------------------------------------
      # Temperature parameters
      # -------------------------------------------------------
      
      A <- (
        input$temp_max -
          input$temp_min
      ) / 2
      
      B <- 2 * pi / 365
      
      D <- (
        input$temp_max +
          input$temp_min
      ) / 2
      
      C <- input$temp_phase
      
      year_growth <- input$temp_growth
      
      daily_growth <- (
        1 + year_growth
      )^(1 / 365) - 1
      
      # -------------------------------------------------------
      # Seasonal temperature
      # -------------------------------------------------------
      
      temp_pred <- (
        A * sin(B * idx + C) + D
      ) *
        exp(
          daily_growth * idx
        )
      
      # -------------------------------------------------------
      # Add noise
      # -------------------------------------------------------
      
      set.seed(input$seed)
      noise = input$temp_noise
      tmaxF = sapply(temp_pred, \(x) 
                     runif(n = 1, min = x - noise, max = x + noise))
      
      # temp_pred = scales::rescale(temp_pred, to = c(0, 100))
      # tmaxF = scales::rescale(tmaxF, to = c(0, 100))
      
      # limit to <= 100 and >= 0
      tmaxF = ifelse(tmaxF > 100, 100, tmaxF)
      tmaxF = ifelse(tmaxF < 0, 0, tmaxF)
      
      # -------------------------------------------------------
      # Return data
      # -------------------------------------------------------
      
      data.table::data.table(
        date = dates,
        city = baseline$city,
        year = lubridate::year(dates),
        idx = idx,
        temp_pred = temp_pred,
        tmaxF = tmaxF
      )
    }
  })
  output$temperature_plot <- renderPlot({
    
    df <- temp_data()
    
    ggplot(
      df,
      aes(
        x = date,
        y = tmaxF
      )
    ) +
      geom_line(alpha = 0.5) +
      geom_line(
        aes(y = temp_pred),
        linewidth = 1,
        color = 'blue'
      ) +
      labs(
        x = NULL,
        y = "Temperature",
        title = "Simulated temperature time series"
      ) +
      theme_minimal()
  })
  
  ## *******************************
  plot1 <- rtPlotServer(
    "plot1",
    x_init = c(0, 30, 60, 100),
    y_init = c(1.04, 1 ,1, 1.04),
    xlab = 'Temperature',
    ylab = 'RR',
    xmin = 0,
    xmax = 100,
    dx = 5,
    ymin = 0.9,
    ymax = 1.1,
    col = 'red'
  )
  
  plot2 <- rtPlotServer(
    "plot2",
    x_init = c(0, 30, 60, 100),
    y_init = c(1.01, 1, 1, 1.01),
    xlab = 'Temperature',
    ylab = 'RR',
    xmin = 0,
    xmax = 100,
    dx = 5,
    ymin = 0.9,
    ymax = 1.1,
    col = 'purple'
  )
  
  plot3 <- rtPlotServer(
    "plot3",
    x_init = c(0, 3, 4, 5),
    y_init = c(1, 0.9, 0.75, 0.6),
    xlab = 'Lag',
    ylab = 'RR',
    xmin = 0,
    xmax = 5,
    dx = 1,
    ymin = 0.5,
    ymax = 1.1,
    col = 'yellow'
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
  
  ####
  argvar <- reactive({
    
    parse_list_input(
      input$argvar_text
    )
  })
  
  ####
  arglag <- reactive({
    
    parse_list_input(
      input$arglag_text
    )
  })
  
  ###
  true_rr <- reactive({
      
      # ---------------------------------------------------
      # Get x and lag dimensions
      # ---------------------------------------------------
      
      x <- plot1$grid()$x
      l <- plot3$grid()$x
      
      maxlag <- input$maxlag
      
      
      # ---------------------------------------------------
      # Create prediction grid
      # ---------------------------------------------------
      
      xpred_base <- tidyr::expand_grid(
        x = x,
        l = l
      )
      
      xpred_base <- xpred_base[, c("x", "l")]
      
      setDT(xpred_base)
      
      setorderv(
        xpred_base,
        "x"
      )
      
      
      # ---------------------------------------------------
      # Create crossbasis
      # ---------------------------------------------------
      
      cp_basis <- dlnm::crossbasis(
        x = xpred_base$x,
        argvar = argvar(),
        arglag = arglag(),
        lag = maxlag
      )
      
      
      # ---------------------------------------------------
      # Create Xpred
      # ---------------------------------------------------
      
      Xpred <- dlnm:::mkXpred(
        "cb",
        cp_basis,
        at = x,
        predvar = x,
        predlag = l,
        cen = input$cen
      )
    
      # ---------------------------------------------------
      # Flatten observed RR surface
      # ---------------------------------------------------
      
      RR <- RRmat_orig()
      
      tRR_mat <- t(RR)
      
      tRR_mat_flat <- matrix(tRR_mat, ncol = 1)
      
      # ---------------------------------------------------
      # Estimate beta
      # ---------------------------------------------------
      
      beta <- MASS::ginv(Xpred) %*% log(tRR_mat_flat)
      
      ## get Cumulative
      ## take from crosspred
      ## i dont think you can get cumse becase you 
      ## don't know the outcomes yet,
      ## and maybe thats ok for the DGM
      ## *** is this a function of how many x points you have ??
      ### ***** ???
      Xpredall <- 0
      cumfit <- matrix(0, length(x), length(l))
      # currMin = Inf
      # xatCurrMin = NA
      
      for (i in seq(length(l))) {
        ind <- seq(length(x)) + length(x) * (i - 1)
        Xpredall <- Xpredall + Xpred[ind, , drop = FALSE]
        cumfit[, i] <- Xpredall %*% beta
        # localMin = cumfit[which.min(cumfit[, i]), i]
        # if(localMin < currMin) {
        #   currMin = localMin
        #   xatCurrMin = x[which.min(cumfit[, i])]
        # }
      }
      
      true_cen = x[which.min(cumfit[, ncol(cumfit)])]
      
      # cout <- exp(cumfit)
      # names(cout) <- c('temp', paste0("lag", l))
      # row.names(cout) <- x
      # write.csv(cout, "cumfit_manual.csv", quote = F)
      # 
      # and then recenter
      # Xpred <- dlnm:::mkXpred(
      #   "cb",
      #   cp_basis,
      #   at = x,
      #   predvar = x,
      #   predlag = l,
      #   cen = xatCurrMin
      # )
      # 
      # Xpredall <- 0
      # cumfit <- matrix(0, length(x), length(l))
      # 
      # for (i in seq(length(l))) {
      #   ind <- seq(length(x)) + length(x) * (i - 1)
      #   Xpredall <- Xpredall + Xpred[ind, , drop = FALSE]
      #   cumfit[, i] <- Xpredall %*% beta
      # }
      
      # ---------------------------------------------------
      # Rebuild RR surface
      # ---------------------------------------------------
      
      rebuild_log_RRmat <- matrix(
        Xpred %*% beta,
        nrow = length(x)
      )
      
      rebuild_RRmat <- exp(
        rebuild_log_RRmat
      )
      
      
      # ---------------------------------------------------
      # Convert to long format
      # ---------------------------------------------------
      
      rebuild_RRmat <- as.data.table(
        rebuild_RRmat
      )
      rebuild_RRmat_out <- rebuild_RRmat
      
      names(rebuild_RRmat) <- as.character(l)
      
      rebuild_RRmat$x <- x
      
      rebuild_RRmat <- melt(
        rebuild_RRmat,
        id.vars = "x"
      )
      
      names(rebuild_RRmat)[2:3] <- c(
        "l",
        "RR"
      )
      
      rebuild_RRmat$l <- type.convert(
        rebuild_RRmat$l,
        as.is = TRUE
      )
      
      
      list(
        beta = beta,
        true_cen = true_cen,
        data = rebuild_RRmat,
        Xpred = Xpred,
        RRmat = rebuild_RRmat_out,
        cumfit = cumfit
      )
    }
  )
  
  RRmat_true <- reactive({ 
    t(true_rr()$RRmat)
  })
  
  combinedRRPlotServer(
    "true_rr_surface",
    RRmat_reactive = RRmat_true,
    x = reactive(plot1$grid()$x),
    l = reactive(plot3$grid()$x)
  )
  
  output$trueRRCumulative <- renderPlot({
    nc = ncol(true_rr()$cumfit)
    df = data.table(
      x = plot1$grid()$x,
      y = exp(true_rr()$cumfit[, nc])
    )
    ggplot(df, aes(x = x, y =y)) +
      geom_hline(yintercept = 1, 
                 linetype = 'dashed') +
      geom_line() + #geom_point() + 
      annotate(geom = 'point', y = 1, x = input$cen,
               color= 'red', shape = 15, size = 5) +
      annotate(geom = 'text',
               x = df$x[1],
               y = max(df$y),
               label = paste0("Cen = ", true_rr()$true_cen)) +
      xlab("Temperature") + 
      ylab("RR") + theme_minimal()
  })
  
  ## ====================================================
  model_results <- reactive({
    
    # -----------------------------------------------------
    # Data
    # -----------------------------------------------------
    
    df_cases <- baseline_cases()
    df_temp  <- temp_data()

    df <- df_cases[
      df_temp, on = c('date', 'city', 'year')
    ]
    
    # -----------------------------------------------------
    # Original crossbasis
    # -----------------------------------------------------
    
    origbasis <- dlnm::crossbasis(
      df$tmaxF,
      argvar = argvar(),
      arglag = arglag(),
      lag = input$maxlag
    )
    
    # # -----------------------------------------------------
    # # Recenter basis
    # # -----------------------------------------------------
    # 
    # basiscen <- onebasis(
    #   x = mean(df$tmaxF)
    #   argvar = argvar(),
    # )
    # 
    # newbasis <- scale(
    #   origbasis,
    #   center = basiscen,
    #   scale = FALSE
    # )
    
    # -----------------------------------------------------
    # Generate updated deaths
    # -----------------------------------------------------
    
    set.seed(input$seed)
    
    error <- rnorm(
      nrow(df),
      sd = input$model_error_sd
    )
    
    deaths_expected_value <- numeric(
      nrow(df)
    )
    
    death_updated <- numeric(
      nrow(df)
    )
    print(input$pois_draw)
    for (i in seq_len(nrow(df))) {
      
      # generating the expcted value
      deaths_expected_value[i] <-
        df$death[i] * exp(sum(origbasis[i, ] * true_rr()$beta)  + error[i])
      
      # and then taking a draw from a poisson distribution
      # using that value --> should you add additional variance

      if(input$pois_draw == TRUE) {
        death_updated[i] <- rpois(
          n = 1,
          lambda = deaths_expected_value[i]
        )
      } else {
        death_updated[i] <- deaths_expected_value[i]
      }
      
      

    }
    
    df$death_updated <- round(death_updated)
    df$death_expected_value <- deaths_expected_value
    print(head(df))
    
    # -----------------------------------------------------
    # Regression model
    # -----------------------------------------------------
    
    m_sub <- gnm::gnm(
      death_updated ~ origbasis,
      data = df,
      family = quasipoisson,
      eliminate = factor(strata)
    )
    
    # -----------------------------------------------------
    # Crossprediction
    # -----------------------------------------------------
    
    cp <- dlnm::crosspred(
      origbasis,
      m_sub,
      cen = min(df$tmaxF),
      by = 1,
      bylag = 1,
      cumul = TRUE
    )
    
    xcen <- cp$predvar[
      which.min(cp$allRRfit)
    ]
    
    cp <- dlnm::crosspred(
      origbasis,
      m_sub,
      cen = xcen,
      by = 1,
      bylag = 1,
      cumul = TRUE
    )
    
    # -----------------------------------------------------
    # Predicted deaths
    # -----------------------------------------------------
    
    death_pred <- exp(
      predict(m_sub)
    )
    
    df$death_pred <- c(
      rep(NA, input$maxlag),
      death_pred
    )
    
    # -----------------------------------------------------
    # Return everything
    # -----------------------------------------------------
    
    list(
      data = df,
      basis = origbasis,
      model = m_sub,
      crosspred = cp,
      xcen = xcen,
      beta = true_rr()$beta,
      beta_estimated = coef(m_sub)
    )
    
  })
  
  output$case_timeseries <- renderPlot({
    
    res <- model_results()
    
    df <- res$data
    
    ggplot(
      subset(df, year(date) == input$case_year)
    ) +
      geom_point(
        aes(
          x = date,
          y = death_updated
        )
      ) +
      geom_line(
        aes(
          x = date,
          y = death_pred
        ),
        linewidth = 0.8,
        col = 'red'
      ) +
      labs(
        x = NULL,
        y = "Deaths",
        title = "Updated vs predicted deaths"
      ) +
      theme_minimal() + 
      coord_cartesian(ylim = c(input$case_ymin, input$case_ymax))
  })
  
  output$temp_timeseries <- renderPlot({
    
    df <- temp_data()
    
    ggplot(
      subset(df, year(date) == input$case_year)
    ) +
      geom_line(
        aes(
          x = date,
          y = tmaxF
        ),
        linewidth = 0.8,
        linetype = '11',
        color = 'purple'
      ) +
      labs(
        x = NULL,
        y = "Daily Max Temperature (F)"
      ) +
      theme_minimal() 
  })
  
  output$rr_lag <- renderPlot({
    
    cp <- model_results()$crosspred
    
    mat <- cp$matRRfit
    
    df <- as.data.table(
      mat,
      keep.rownames = "temperature"
    )
    
    df <- melt(
      df,
      id.vars = "temperature"
    )
    
    names(df) <- c(
      "temperature",
      "lag",
      "RR"
    )
    
    df$temperature <- as.numeric(
      df$temperature
    )
    
    df$lag <- as.numeric(
      gsub("lag", "", as.character(df$lag))
    )
    
    df$temperature <- as.numeric(df$temperature)
    
    setDT(df)
    
    df[, temp_5 := round(temperature / 5) * 5]
    
    df <- df[
      , .SD[
        which.min(abs(temperature - temp_5[1]))
      ],
      by = .(temp_5, lag)
    ]
    
    df[, temperature := temp_5]
    df[, temp_5 := NULL]
    
    ggplot(
      df,
      aes(
        x = lag,
        y = RR,
        group = temperature,
        color = temperature
      )
    ) +
      geom_line(
        alpha = 0.5
      ) +
      geom_hline(
        yintercept = 1,
        linetype = "dashed"
      ) +
      labs(
        x = "Lag",
        y = "Relative Risk"
      ) +
      theme_minimal()
  })
  
  output$model_coefficients <- renderTable({
    
    res <- model_results()
    
    data.frame(
      Term = names(res$beta_estimated),
      Estimated = as.numeric(
        res$beta_estimated
      ),
      True = as.numeric(
        res$beta
      )
    )
    
  },
  digits = 4)
  
  output$model_summary <- renderPrint({
    
    summary(
      model_results()$model
    )
  })
  
  output$temp_summary <- renderPrint({
    
    summary(
      temp_data()$tmaxF
    )
  })
  
  observeEvent(input$baseline, {
    
    updateNumericInput(
      session,
      "case_ymin",
      value = input$baseline * 0.5
    )
    
    updateNumericInput(
      session,
      "case_ymax",
      value = input$baseline * 1.5
    )
    
  })
  
  output$rr_overall <- renderPlot({
    
    cp <- model_results()$crosspred
    
    df <- data.frame(
      x = cp$predvar,
      RR = cp$allRRfit,
      low = cp$allRRlow,
      high = cp$allRRhigh
    )
    
    nc = ncol(true_rr()$cumfit)
    df_true = data.table(
      x = plot1$grid()$x,
      y = exp(true_rr()$cumfit[, nc])
    )

    ggplot(df, aes(x = x, y = RR)) +
      geom_line(data = df_true,
                aes(x = x, y = y), 
                col = 'blue') +
      geom_ribbon(
        aes(
          ymin = low,
          ymax = high
        ),
        alpha = 0.2
      ) +
      geom_line() +
      geom_hline(
        yintercept = 1,
        linetype = "dashed"
      ) +
      annotate(geom = 'text',
               x = cp$predvar[1],
               y = max(cp$allRRhigh),
               label = paste0("Cen = ", cp$cen)) +
      labs(
        x = "Temperature",
        y = "Relative Risk"
      ) +
      theme_minimal()
  })
  
  joined_data <- reactive({
    
    df_cases <- baseline_cases()
    df_temp  <- temp_data()
    
    df <- df_cases[
      df_temp,
      on = c("date", "city", "year")
    ]
    
    df
  })
  
  output$download_csv <- downloadHandler(
    
    filename = function() {
      paste0(
        "sample_data_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      data.table::fwrite(
        joined_data(),
        file
      )
    }
  )
  
}

shinyApp(
  ui = ui,
  server = server
)