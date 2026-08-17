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
              max = 1,
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
      "Model Results",
      
      helpText("The baseline case counts are modified according to the specified relative-risk surface to create an updated set of cases. A small amount of random variation is added to the simulated values. These updated cases are then analyzed using a quasi-Poisson regression model to estimate the temperature–risk relationship and assess how closely the fitted model recovers the specified relative-risk surface."),
      
      fluidRow(
        
        column(
          width = 3,
          numericInput("case_year",
                       "Case Year",
                       value = 1980,
                       min = 1981, max = 1999, step = 1)
        ),
        
        column(
          width = 3,
          numericInput("model_error_sd",
                       "Model error",
                       value = 0.1, min = 0.0000001)
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
    dx = 5,
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
    dx = 5,
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
    dx = 1,
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
      
      cout <- exp(cumfit)
      names(cout) <- c('temp', paste0("lag", l))
      row.names(cout) <- x
      write.csv(cout, "cumfit_manual.csv", quote = F)
      
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
    # Centering temperature
    # -----------------------------------------------------
    
    x_cen <- which.min(
      abs(
        df$tmaxF - input$cen
      )
    )
    
    # -----------------------------------------------------
    # Original crossbasis
    # -----------------------------------------------------
    
    origbasis <- dlnm::crossbasis(
      df$tmaxF,
      argvar = argvar(),
      arglag = arglag(),
      lag = input$maxlag
    )
    
    # -----------------------------------------------------
    # Recenter basis
    # -----------------------------------------------------
    
    basiscen <- origbasis[x_cen, ]
    
    newbasis <- scale(
      origbasis,
      center = basiscen,
      scale = FALSE
    )
    
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
    
    for (i in seq_len(nrow(df))) {
      
      # generating the simuluated data
      deaths_expected_value[i] <-
        df$death[i] * exp(sum(newbasis[i, ] * true_rr()$beta)  + error[i])
      
      # death_updated[i] <- rpois(
      #   n = 1,
      #   lambda = deaths_expected_value[i]
      # )
      
      death_updated[i] <- deaths_expected_value[i]

    }
    
    df$death_updated <- death_updated
    
    # -----------------------------------------------------
    # Regression model
    # -----------------------------------------------------
    
    m_sub <- gnm::gnm(
      death_updated ~ newbasis,
      data = df,
      family = quasipoisson,
      eliminate = factor(strata)
    )
    
    # -----------------------------------------------------
    # Crossprediction
    # -----------------------------------------------------
    
    cp <- dlnm::crosspred(
      newbasis,
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
      newbasis,
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
      basis = newbasis,
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
      temp_data()
    )
  })
  
  output$rr_overall <- renderPlot({
    
    cp <- model_results()$crosspred
    
    write.csv(exp(cp$cumfit), "cumfit_auto.csv")

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
  
}

shinyApp(
  ui = ui,
  server = server
)