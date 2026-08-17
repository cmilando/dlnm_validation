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
      
      helpText("This page allows the user to define the baseline population,
               along with year trends, day of week trends."),
      
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
    
    tabPanel(
      "Temperature",
      
      helpText("This page reads in the temperature file listed below"),
      
      fluidRow(
        
        # -----------------------------------------------------
        # Temperature file
        # -----------------------------------------------------
        
        column(
          width = 6,
          
          div(
            class = "plot-container",
            
            h4(
              "Temperature Data",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            p(
              strong("File: "),
              "getdailystat73.149.185.213.228.6.19.51"
            ),
            
            p(
              "Daily maximum temperature (°F), 1980–1999."
            )
          )
        ),
        
        
        # -----------------------------------------------------
        # City
        # -----------------------------------------------------
        
        column(
          width = 6,
          
          div(
            class = "plot-container",
            
            h4(
              "Location",
              style = "margin-top: 0; margin-bottom: 10px;"
            ),
            
            textInput(
              "temp_city",
              "City",
              value = "BOSTON",
              width = "100%"
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
        
      )
    ),
    
    # -----------------------------------------------------
    # Relative Risk
    # -----------------------------------------------------
    
    tabPanel(
      "Create RR surface",
      
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
      "True RR Surface",
      
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
              value = 50,
              step = 0.1,
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
      
      plotOutput("trueRRCumulative",               
                 width = "100%",
                 height = "300px")
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
  
  temp_data <- reactive({
    
    temp_data <- read.table(
      "getdailystat73.149.185.213.228.6.19.51",
      sep = ",",
      colClasses = c(
        "numeric",
        rep("character", 3)
      )
    )
    
    names(temp_data) <- c(
      "tmaxF",
      "year",
      "month",
      "day"
    )
    
    setDT(temp_data)
    
    temp_data[
      ,
      dtstr := paste0(
        trimws(year), "-",
        trimws(month), "-",
        trimws(day)
      )
    ]
    
    temp_data[
      ,
      date := as.IDate(dtstr)
    ]
    
    temp_data[
      ,
      c("month", "day", "dtstr") := NULL
    ]
    
    temp_data[
      ,
      year := year(date)
    ]
    
    temp_data[
      ,
      city := input$temp_city
    ]
    
    temp_data
  })
  
  output$temperature_plot <- renderPlot({
    
    df <- temp_data()
    
    plot(
      df$date,
      df$tmaxF,
      type = "l",
      xlab = "Date",
      ylab = "Maximum temperature (°F)"
    )
  })
  
  ## *******************************
  plot1 <- rtPlotServer(
    "plot1",
    x_init = c(0, 30, 60, 100),
    y_init = c(1, 1 ,1, 1.04),
    xlab = 'Temperature',
    ylab = 'RR',
    xmin = 0,
    xmax = 100,
    dx = 1,
    ymin = 0.9,
    ymax = 1.1
  )
  
  plot2 <- rtPlotServer(
    "plot2",
    x_init = c(0, 30, 60, 100),
    y_init = c(1, 1, 1, 1.01),
    xlab = 'Temperature',
    ylab = 'RR',
    xmin = 0,
    xmax = 100,
    dx = 1,
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
      Xpredall <- 0
      cumfit <- matrix(0, length(x), length(l))
      currMin = Inf
      xatCurrMin = NA
      
      for (i in seq(length(l))) {
        ind <- seq(length(x)) + length(x) * (i - 1)
        Xpredall <- Xpredall + Xpred[ind, , drop = FALSE]
        cumfit[, i] <- Xpredall %*% beta
        localMin = cumfit[which.min(cumfit[, i]), i]
        if(localMin < currMin) {
          currMin = localMin
          xatCurrMin = x[which.min(cumfit[, i])]
        }
      }
      
      # and then recenter
      Xpred <- dlnm:::mkXpred(
        "cb",
        cp_basis,
        at = x,
        predvar = x,
        predlag = l,
        cen = xatCurrMin
      )
      
      Xpredall <- 0
      cumfit <- matrix(0, length(x), length(l))
      
      for (i in seq(length(l))) {
        ind <- seq(length(x)) + length(x) * (i - 1)
        Xpredall <- Xpredall + Xpred[ind, , drop = FALSE]
        cumfit[, i] <- Xpredall %*% beta
      }
      
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
    plot(x = plot1$grid()$x,
         y = exp(true_rr()$cumfit[, nc]))
  })
  
}

shinyApp(
  ui = ui,
  server = server
)