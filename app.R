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
library(MASS)

##
source("app_fcns.R")
source("calc_vcov.R")
source("calc_dispersion.R")

##
source("UI_info.R")
source("UI_population.R")
source("UI_temperature.R")
source("UI_RRsurface.R")
source("UI_trueRR.R")
source("UI_model.R")
source("UI_download.R")

##

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
    
    infoPanel,
    populationPanel,
    temperaturePanel,
    RRsurfacePanel,
    trueRRPanel,
    modelPanel,
    downloadPanel
   )
  )
)



server <- function(input, output, session) {
  
  ## *******************************
  observe({
    
    req(input$baseline_yr, input$end_yr)
    
    updateDateRangeInput(
      session,
      "xRange",
      start = as.Date(
        paste0(input$baseline_yr, "-01-01")
      ),
      end = as.Date(
        paste0(input$baseline_yr, "-12-31")
      )
    )
  })
  
  baseline_cases <- reactive({
    
    get_baseline_cases(
      baseline = input$baseline,
      rand = input$rand,
      year_growth = input$year_growth,
      dow_effect = input$dow_effect,
      season_effect = input$season_effect,
      season_phase = input$season_phase,
      city = input$city,
      baseline_yr = input$baseline_yr,
      end_yr = input$end_yr,
      seed = input$seed
    )
  })
  
  output$baseline_cases_plot <- renderPlot({
    
    df <- baseline_cases()

    df <- subset(df,
                 date >= as.IDate(input$xRange[1]) &
                 date <= as.IDate(input$xRange[2]))
    
    
    ggplot(df) + 
      geom_line(aes(x = date,
                    y = death)) + 
      theme_minimal() + 
      labs(
        x = NULL,
        y = 'Baseline Deaths'
      )
    
  })
  
  ## *******************************
  
  shinyjs::disable("temp_min")
  shinyjs::disable("temp_max")
  shinyjs::disable("true_maxlag")
  #shinyjs::disable("model_maxlag")
  
  observeEvent(input$use_station_data, {
    
    if (input$use_station_data) {
      
      shinyjs::disable("amplitude_min")
      shinyjs::disable("amplitude_max")
      shinyjs::disable("temp_growth")
      shinyjs::disable("temp_noise")
      
    } else {
      
      shinyjs::enable("amplitude_min")
      shinyjs::enable("amplitude_max")
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
        input$amplitude_max -
          input$amplitude_min
      ) / 2
      
      # daily
      B <- 2 * pi / 365.25
      
      D <- (
        input$amplitude_max +
          input$amplitude_min
      ) / 2
      
      # hard coded the phase shift for cold in winter
      C <- 4.44
      
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
      
      # limit to upper and lower bounds
      tmaxF = ifelse(tmaxF > input$temp_max, input$temp_max, tmaxF)
      tmaxF = ifelse(tmaxF < input$temp_min, input$temp_min, tmaxF)
      
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
      theme_minimal() + 
      coord_cartesian(ylim = c(input$temp_min, input$temp_max))
  })
  
  ## *******************************
  RRplot_xcoords <- c(0, 30, 60, 100) ## TODO: update to input$temp_max
  RRplot_lagcoords <- c(0, 3, 4, 5)   ## TODO: update to input$maxlag
  
  plot1 <- rtPlotServer(
    "plot1",
    x_init = RRplot_xcoords,
    y_init = c(1.012, 1 ,1, 1.04),
    xlab = 'Temperature',
    ylab = 'RR',
    xmin = 0,   ## TODO: update to input$temp_min
    xmax = 100, ## TODO: update to input$temp_max
    dx = 5,     ## TODO: update to 5% of difference
    ymin = 0.5,
    ymax = 2.0,
    col = 'red',
    islag = FALSE
  )
  
  plot2 <- rtPlotServer(
    "plot2",
    x_init = RRplot_xcoords,
    y_init = c(1.01, 1, 1, 1.01),
    xlab = 'Temperature',
    ylab = 'RR',
    xmin = 0,   ## TODO: update to input$temp_min
    xmax = 100, ## TODO: update to input$temp_max
    dx = 5,     ## TODO: update to 5% of difference
    ymin = 0.5,
    ymax = 2.0,
    col = 'purple',
    islag = FALSE
  )
  
  plot3 <- rtPlotServer(
    "plot3",
    x_init = RRplot_lagcoords,
    y_init = c(1, 0.9, 0.75, 0.6),
    xlab = 'Lag',
    ylab = 'RR',
    xmin = 0,
    xmax = 5, ## TODO: update to input$maxlag
    dx = 1,
    ymin = 0.0,
    ymax = 1.1,
    col = 'yellow',
    islag = TRUE
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
  true_argvar <- reactive({
    
    parse_list_input(
      input$true_argvar_text
    )
  })
  
  model_argvar <- reactive({
    
    parse_list_input(
      input$model_argvar_text
    )
  })
  
  ####
  true_arglag <- reactive({
    
    parse_list_input(
      input$true_arglag_text
    )
  })
  
  model_arglag <- reactive({
    
    parse_list_input(
      input$model_arglag_text
    )
  })
  
  ###
  true_rr <- reactive({
      
      # ---------------------------------------------------
      # Get x and lag dimensions
      # ---------------------------------------------------
      
      x <- plot1$grid()$x
      l <- plot3$grid()$x
      maxlag <- input$true_maxlag
      
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
        argvar = true_argvar(),
        arglag = true_arglag(),
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
        cen = input$true_cen
      )
          
      # ---------------------------------------------------
      # Flatten observed RR surface
      # ---------------------------------------------------
      
      RR <- RRmat_orig()
      
      tRR_mat <- t(RR)
      
      tRR_mat_flat <- matrix(tRR_mat, ncol = 1)
      
      # ---------------------------------------------------
      # Estimate beta,
      # ---------------------------------------------------
      
      beta <- MASS::ginv(Xpred) %*% log(tRR_mat_flat)
      
      # ---------------------------------------------------
      # Get cumulative
      # ---------------------------------------------------
      
      ## get Cumulative
      ## take from crosspred
      ## i dont think you can get cumse becase you 
      ## don't know the outcomes yet,
      ## and maybe thats ok for the DGM
      Xpredall <- 0
      cumfit <- matrix(0, length(x), length(l))

      for (i in seq(length(l))) {
        ind <- seq(length(x)) + length(x) * (i - 1)
        Xpredall <- Xpredall + Xpred[ind, , drop = FALSE]
        cumfit[, i] <- Xpredall %*% beta
      }
      
      true_cen = x[which.min(cumfit[, ncol(cumfit)])]
      
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
      annotate(geom = 'point', y = 1, x = input$true_cen,
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
      df_temp, on = c('date', 'city', 'year', 'idx')
    ]

    # -----------------------------------------------------
    # Generate updated deaths
    # -----------------------------------------------------
    
    set.seed(input$seed)
    
    ## you want all deaths predicted you need to add addtional data
    add_extra_T <- 1:input$true_maxlag
 
    true_basis <- dlnm::crossbasis(
      c(df$tmaxF[add_extra_T], df$tmaxF), 
      argvar = true_argvar(),
      arglag = true_arglag(),
      lag = input$true_maxlag
    )

    # then reset it
    true_basis <- data.frame(true_basis[(input$true_maxlag + 1):nrow(true_basis), ])
    
    ## TODO: 
    ## -- calculate VCOV
    ## -- calc dispersion and updated VCOV
    ## -- then, sample from beta and vcov using MVNORM, so each day gets its own
    ##    beta
    ## the problem is, the way I'm generating beta doesn't 
    ## allow me to get a confidence interval
    ## 
    ## xvcov <- calc_vcov(df)


    
    deaths_expected_value <- numeric(nrow(df))
    death_updated <- numeric(nrow(df))

    for (i in 1:nrow(df)) {
      
      # generating the expcted value
      deaths_expected_value[i] <-
        df$death[i] * exp(sum(true_basis[i, ] * true_rr()$beta))
      
    }
    
    # and then taking a draw from a poisson distribution
    # using that value --> should you add additional variance
    
    if(input$pois_draw == TRUE) {
      death_updated <- rpois(
        n = length(deaths_expected_value),
        lambda = deaths_expected_value
      )
    } else {
      death_updated <- deaths_expected_value
    }
    
    df$death_updated <- round(death_updated)
    df$death_expected_value <- deaths_expected_value
  
    # -----------------------------------------------------
    # Regression model
    # -----------------------------------------------------
    
    # now update the crossbasis to be the one from the model block
    cb_model <- dlnm::crossbasis(
      df$tmaxF,
      argvar = model_argvar(),
      arglag = model_arglag(),
      lag = input$model_maxlag
    )
    
    m_sub <- gnm::gnm(
      death_updated ~ cb_model,
      data = df,
      family = quasipoisson,
      eliminate = factor(strata)
    )
    
    # -----------------------------------------------------
    # Crossprediction
    # -----------------------------------------------------
    
    cp <- dlnm::crosspred(
      cb_model,
      m_sub,
      cen = min(df$tmaxF),
      by = 1,     ## this can in theory be < 1
      bylag = 1,  ## this can in theory be < 1
      cumul = TRUE
    )
    
    xcen <- cp$predvar[
      which.min(cp$allRRfit)
    ]
    
    cp <- dlnm::crosspred(
      cb_model,
      m_sub,
      cen = input$model_cen,
      by = 1,     ## this can in theory be < 1
      bylag = 1,  ## this can in theory be < 1
      cumul = TRUE
    )
    
    # -----------------------------------------------------
    # Predicted deaths
    # -----------------------------------------------------
    
    death_pred <- exp(
      predict(m_sub)
    )
    
    df$death_pred <- c(
      rep(NA, input$model_maxlag),
      death_pred
    )
    
    df <- subset(df, !is.na(death_pred))
    
    if(any(df$death_updated == 0)) {
      rr <- which(df$death_updated == 0)
      print(df[rr, ])
    }
    
    # -----------------------------------------------------
    # Return everything
    # -----------------------------------------------------
    
    list(
      data = df,
      basis = cb_model,
      model = m_sub,
      crosspred = cp,
      xcen = xcen,
      beta = true_rr()$beta,
      beta_estimated = coef(m_sub)
    )
    
  })
  
  output$full_case_timeseries <- renderPlot({
    
    res <- model_results()
    
    df <- res$data
    
    ggplot(df) +
      geom_point(
        aes(
          x = date,
          y = death_updated
        )
      ) +
      labs(
        x = NULL,
        y = "Deaths",
        title = "Updated deaths"
      ) +
      theme_minimal() +
      coord_cartesian(ylim = c(input$full_case_ymin, input$full_case_ymax))
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
  
  output$codeSnip <- renderText({
    
    "
    # Necessary libraries
    library(data.table)
    library(dlnm)
    
    # Read in data from saved file
    df <- read.csv('sample_data_2026-08-19.csv')
    df <- setDT(df)
    
    # crossbasis
    argvar = list(fun = 'ns', knots = c(50, 70))
    arglag = list(fun = 'ns', knots = c(2))
    maxlag = 5
    
    cb_model <- dlnm::crossbasis(
      df$tmaxF,
      argvar = argvar,
      arglag = arglag,
      lag = maxlag
    )
    
    m_sub <- gnm::gnm(
      death_updated ~ cb_model,
      data = df,
      family = quasipoisson,
      eliminate = factor(strata)
    )
    
    # crosspred
    cp <- dlnm::crosspred(
      cb_model,
      m_sub,
      cen = min(df$tmaxF),
      by = 1,     ## this can in theory be < 1
      bylag = 1,  ## this can in theory be < 1
      cumul = TRUE
    )
    
    xcen <- cp$predvar[
      which.min(cp$allRRfit)
    ]
    
    cp <- dlnm::crosspred(
      cb_model,
      m_sub,
      cen = xcen,
      by = 1,     ## this can in theory be < 1
      bylag = 1,  ## this can in theory be < 1
      cumul = TRUE
    )
    
    # Set up 2x2 plotting area
    par(mfrow = c(1, 3))
    
    # 1. 3D temperature-lag-RR plot
    plot(cp,
         xlab = 'Temperature',
         zlab = 'Relative Risk')

    # 2. Overall exposure-response
    plot(cp, 'overall',
         main = 'Cumulative Risk',
         xlab = 'Temperature',
         ylab = 'Relative Risk')
    
    
    # 3. Observed vs predicted deaths
    death_pred <- exp(predict(m_sub))
    
    df$death_pred <- c(rep(NA, maxlag), death_pred)
    df <- subset(df, !is.na(death_pred))
    
    x <- df$death_updated
    y <- df$death_pred
    
    # Remove missing values
    ok <- complete.cases(x, y)
    x <- x[ok]
    y <- y[ok]
    
    # Statistics
    r  <- cor(x, y)
    r2 <- r^2
    
    plot(x, y,
         xlab = 'Observed deaths',
         ylab = 'Predicted deaths')
    
    # 1:1 line
    abline(a = 0, b = 1, lty = 2, col = 'red')
    
    # Regression line
    fit <- lm(y ~ x)
    abline(fit, lwd = 2, col = 'purple')
    
    # Correlation / R² annotation
    legend('topleft', 
       legend = sprintf('r = %.2f,  R² = %.2f',
                         r, r2), bty = 'n')

    "
    
  })
  
  output$case_scatter <- renderPlot({
    
    res <- model_results()
    
    df <- res$data
    
    ggplot(
      subset(df, year(date) == input$case_year)
    ) +
      geom_point(
        aes(
          x = death_updated,
          y = death_pred,
          color = factor(month(date), ordered = T,
                         levels = 1:12)
        )
      ) +
      scale_color_manual(name = 'Month', values = c(
        '1' = 'blue', '2' = 'blue', '11' = 'blue', '12' = 'blue',
        '3' = 'green', '4' = 'green', 
        '5' = 'red', '6' = 'red', '7' = 'red', '8' = 'red', '9' = 'red',
        '10' = 'purple'
      )) +
      geom_abline(slope = 1, intercept = 0) +
      labs(
        x = "Observed Deaths",
        y = "Predicted Deaths",
        title = "Updated vs predicted deaths"
      ) +
      theme_minimal() +
      coord_cartesian(ylim = c(input$case_ymin, input$case_ymax),
                      xlim = c(input$case_ymin, input$case_ymax))
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
    
    df[, temp_5 := round(temperature / 5) * 5]  ## TODO: make sure this works
    
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
      "full_case_ymin",
      value = input$baseline * 0.25
    )
    
    updateNumericInput(
      session,
      "case_ymax",
      value = input$baseline * 1.5
    )
    
    updateNumericInput(
      session,
      "full_case_ymax",
      value = input$baseline * 2.0
    )
    
  })
  
  output$rr_overall <- renderPlot({
    
    cp <- model_results()$crosspred
    xcen <- model_results()$xcen
    
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
               label = paste0("True Cen = ", xcen)) +
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
        model_results()$data,
        file
      )
    }
  )
  
}

shinyApp(
  ui = ui,
  server = server
)