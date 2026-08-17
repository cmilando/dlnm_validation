local_rescale <- function(xvec, scalefirst, scalelast) {
  ## another way of saying this is I want scalefirst
  ## and scalelast to be the vector, following the shape of xvec
  ## and allow it go above scalefirst if xvec > 1
  xout <- numeric(length(xvec))
  xrange <- abs(xvec[1] - xvec[length(xvec)])
  scalerange = scalefirst - scalelast
  for(ii in 1:length(xvec)) {
    xout[ii] = (xvec[ii] - xvec[length(xvec)])/xrange*scalerange + scalelast
    #xout[ii] = (xvec[i] - xvec[length(xvec)])/xrange*scalerange + scalefirst
  }
  
  return(xout)
}

# ---------------------------------------------------------
# Reusable plot module UI
# ---------------------------------------------------------

rtPlotUI <- function(id, title = NULL, helpText = NULL) {
  
  ns <- NS(id)
  
  tagList(
    
    if (!is.null(title)) {
      h4(title)
    },
    
    if (!is.null(helpText)) {
      helpText(helpText)
    },
    
    sliderInput(
      ns("BSDEG"),
      "Spline Degree:",
      value = 3,
      min = 1,
      max = 4,
      step = 1,
      width = "90%"
    ),
    
    plotlyOutput(
      ns("plot"),
      width = "100%",
      height = "260px"
    )
  )
}


# ---------------------------------------------------------
# Reusable plot module server
# ---------------------------------------------------------

rtPlotServer <- function(
    id,
    x_init,
    y_init,
    ylab,
    xlab,
    xmin,
    xmax,
    dx,
    ymin,
    ymax
) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ---------------------------------------------------
      # Module-specific state
      # ---------------------------------------------------
      
      rv <- reactiveValues(x = x_init, y = y_init)
      NPTS <- length(x_init)
      
      # ---------------------------------------------------
      # Linear model through draggable points
      # ---------------------------------------------------
      
      linear.model <- reactive({
        
        d <- data.frame(x = rv$x, y = rv$y)
        lm(y ~ bs(x, degree = 1, knots = rv$x), data = d)
        
      })
      
      # ---------------------------------------------------
      # Generate daily R(t) values
      # ---------------------------------------------------
      
      rt_grid <- reactive({
        
        this.x <- seq(xmin, xmax, by = dx)
        
        suppressWarnings(
          this.y <- predict(
            linear.model(),
            newdata = data.frame(x = this.x)
          )
        )
        
        # B-spline basis
        X_mat <- bs(x = this.x, knots = rv$x, degree = input$BSDEG)
        
        # Fit spline to linear interpolation
        m2 <- lm(this.y ~ X_mat)
        
        yspline <- predict(m2)
        
        data.frame(x = this.x, y = as.vector(yspline))
      })
      
      
      # ---------------------------------------------------
      # Render Plotly
      # ---------------------------------------------------
      
      output$plot <- renderPlotly({
        
        req(rv$x, rv$y)
        
        grid <- rt_grid()
        
        req(grid$x, grid$y)
        
        # -----------------------------------------------
        # Create draggable circles
        # -----------------------------------------------
        
        circles <- map2(
          rv$x,
          rv$y,
          
          ~ list(
            type = "circle",
            
            xanchor = .x,
            yanchor = .y,
            
            x0 = -4,
            x1 = 4,
            y0 = -4,
            y1 = 4,
            
            xsizemode = "pixel",
            ysizemode = "pixel",
            
            fillcolor = "red",
            
            line = list(
              color = "black",
              width = 1
            )
          )
        )
        
        
        # -----------------------------------------------
        # Plot
        # -----------------------------------------------
        
        plot_ly(
          source = session$ns("plot")
        ) %>%
          
          # 1.0 line
          add_lines(
            x = c(xmin, xmax),
            y = c(1, 1),
            color = I("black"),
            line = list(
              width = 0.5,
              dash = "dot"
            ),
            showlegend = FALSE
          ) %>%
          
          # red line
          add_lines(
            x = grid$x,
            y = grid$y,
            color = I("#F89880"),
            showlegend = FALSE,
            hoverinfo = "y",
            hovertemplate = "%{y:.3f}<extra></extra>"
          ) %>%
          
          layout(
            
            plot_bgcolor = "#f0f0f0",
            
            xaxis = list(
              title = xlab,
              zerolinecolor = "#f0f0f0",
              gridcolor = "#ffffff",
              titlefont = list(
                family = "Arial, sans-serif"
              ),
              tickfont = list(
                family = "Arial, sans-serif"
              ),
              range = c(
                xmin - 1,
                xmax + 1
              )
            ),
            
            yaxis = list(
              title = ylab,
              range = c(ymin, ymax),
              zerolinecolor = "#f0f0f0",
              gridcolor = "#ffffff",
              titlefont = list(
                family = "Arial, sans-serif"
              ),
              tickfont = list(
                family = "Arial, sans-serif"
              )
            ),
            
            margin = list(
              l = 50,
              r = 20,
              b = 50,
              t = 20
            ),
            
            shapes = circles,
            
            font = list(
              family = "Arial, sans-serif"
            )
          ) %>%
          
          config(
            edits = list(
              shapePosition = TRUE
            )
          )
      })
      
      
      # ---------------------------------------------------
      # Capture draggable shape movements
      # ---------------------------------------------------
      
      observe({
        
        ed <- event_data(
          "plotly_relayout",
          source = session$ns("plot")
        )
        
        req(ed)
        
        shape_anchors <- ed[
          grepl(
            "^shapes.*anchor$",
            names(ed)
          )
        ]
        
        if (length(shape_anchors) != 2) {
          return()
        }
        
        row_index <- unique(
          readr::parse_number(
            names(shape_anchors)
          ) + 1
        )
        
        pts <- as.numeric(shape_anchors)
        
        if (
          length(row_index) != 1 ||
          row_index < 1 ||
          row_index > NPTS
        ) {
          return()
        }
        
        
        # -----------------------------------------------
        # X bounds
        # -----------------------------------------------
        
        if (row_index == 1) {
          
          rv$x[row_index] <- xmin
          
        } else if (row_index == NPTS) {
          
          rv$x[row_index] <- xmax
          
        } else {
          
          rv$x[row_index] <- max(
            xmin,
            min(xmax, pts[1])
          )
        }
        
        
        # -----------------------------------------------
        # Y bounds
        # -----------------------------------------------
        
        rv$y[row_index] <- max(
          ymin,
          min(ymax, pts[2])
        )
      })
      
      
      # Return reactive values if the parent needs them
      return(
        list(
          points = reactive({
            data.frame(
              x = rv$x,
              y = rv$y
            )
          }),
          grid = rt_grid
        )
      )
    }
  )
}



combinedRRPlotUI <- function(id, title = "Combined RR") {
  
  ns <- NS(id)
  
  tagList(
    
    h4(
      title,
      style = "margin-top: 0; margin-bottom: 8px;"
    ),
    
    fluidRow(
      
      # -----------------------------------------------
      # Orientation controls
      # -----------------------------------------------
      
      column(
        width = 3,
        
        div(
          class = "plot-container",
          
          numericInput(
            ns("pitch"),
            "Pitch",
            180,
            min = -360,
            max = 360,
            step = 1,
            width = "100%"
          ),
          
          numericInput(
            ns("yaw"),
            "Yaw",
            126,
            min = -360,
            max = 360,
            step = 1,
            width = "100%"
          ),
          
          numericInput(
            ns("roll"),
            "Roll",
            104,
            min = -360,
            max = 360,
            step = 1,
            width = "100%"
          )
        )
      ),
      
      # -----------------------------------------------
      # Plot
      # -----------------------------------------------
      
      column(
        width = 9,
        
        div(
          class = "plot-container",
          
          plotOutput(
            ns("plot"),
            width = "100%",
            height = "350px"
          )
        )
      )
    )
  )
}

combinedRRPlotServer <- function(
    id,
    RRmat_reactive,
    x,
    l
) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$plot <- renderPlot({
        
        # Get current RR matrix
        RR <- RRmat_reactive()
        
        # -------------------------------------------------
        # Build data frame
        # -------------------------------------------------
        
        RR_df <- as.data.table(RR)
        
        RR_df$l <- l()
        
        names(RR_df) <- as.character(
          c(x(), "l")
        )
        
        RR_df <- melt(
          RR_df,
          id.vars = "l",
          variable.factor = FALSE
        )
        
        RR_df$variable <- type.convert(
          RR_df$variable,
          as.is = TRUE
        )
        
        names(RR_df)[2:3] <- c(
          "x",
          "RR"
        )
        
        # -------------------------------------------------
        # 3D surface
        # -------------------------------------------------
        
        ggplot(RR_df) +
          geom_surface_3d(
            mapping = aes(
              x = x,
              y = l,
              z = RR,
              fill = RR
            )
          ) +
          coord_3d(
            pitch = input$pitch,
            yaw = input$yaw,
            roll = input$roll
          ) +
          scale_fill_gradient2()
      })
    }
  )
}

# set cases based on a year trend
get_baseline_cases <- function(
    baseline,
    sd,
    year_growth,
    dow_effect,
    city,
    baseline_yr = 1980,
    end_yr = 1999,
    seed = 123
) {
  
  set.seed(seed)
  
  # make the skeleton you need later
  get_year_dt <- function(yy) {
    st = lubridate::make_date(yy, 1, 1)
    ed = lubridate::make_date(yy, 12, 31)
    dt = seq.Date(st, ed, by = 'day')
    return(dt)
  }
  
  all_dt <- lapply(baseline_yr:end_yr, get_year_dt)
  all_dt <- do.call(c, all_dt)
  
  # make dt
  df <- data.table(date = as.IDate(all_dt), city = city)
  df[,year := year(all_dt)]
  df[,dow := wday(all_dt)]
  df[,month := month(all_dt)]
  df[,idx := 1:nrow(df)]
  
  # add year trend but compounded daily
  # (1 + Growth Rate)^(1/365)-1
  daily_growth = (1 + year_growth)^(1/365) - 1
  df[,death := baseline * exp(daily_growth * idx)]
  
  # add day of week trend
  dow_sine <- function(x) {
    A = dow_effect # amplitude
    B = 2*pi/7 # frequency
    C = 0 # phase shift
    D = 1 # vertical shift
    A * sin(B*x + C) + D
  }
  # plot(dow_sine(1:7), type = 'l')
  # points(dow_sine(1:7))
  df[,death := death * dow_sine(idx)]
  
  # add noise based on sd
  # applied in log space to make sure its positive
  noise = rnorm(nrow(df), sd = sd)
  df[, death := exp(log(death) + noise)]
  
  # round to integer
  df[, death := round(death)]
  
  # add strata
  df[, strata := paste0(city, ":", year, ":", month, ":", dow)]
  
  return(df)
  
}


parse_list_input <- function(x) {
  
  expr <- parse(text = x)
  
  if (length(expr) != 1) {
    stop("Input must contain exactly one R expression.")
  }
  
  out <- eval(expr[[1]], envir = baseenv())
  
  if (!is.list(out)) {
    stop("Input must evaluate to a list().")
  }
  
  out
}

calc_vcov <- function(y, X, beta, stratum_vector) {
  # X      : n x p design matrix (no stratum intercepts)
  # beta   : length-p coefficient vector
  # strata : length-n vector defining conditioning strata
  
  # validation
  stopifnot(
    is.matrix(X),
    length(y) == nrow(X),
    length(beta) == ncol(X),
    length(stratum_vector) == nrow(X)
  )
  
  # Recode strata as consecutive integers 1, ..., S
  n_strata <- length(unique(stratum_vector))
  stratum_vector <- as.numeric(factor(stratum_vector, labels = 1:n_strata))
  
  # Initialize the Fisher information matrix for beta
  # Dimension: p x p, where p = number of regression coefficients
  # We will accumulate information across strata
  p <- ncol(X)
  I <- matrix(0, p, p)
  
  # Loop over strata
  # Each iteration adds the Fisher information contribution
  # from one multinomial likelihood (one conditional Poisson stratum)
  for (s in seq_len(n_strata)) {
    
    # Identify which observations belong to stratum s
    # These observations share a fixed total count
    idx <- which(stratum_vector == s)
    
    # Skip degenerate strata
    if (length(idx) <= 1) next
    
    # Extract the design matrix rows for stratum s
    # Dimension: n_s x p
    # drop = FALSE ensures Xs stays a matrix even if n_s = 1
    Xs <- X[idx, , drop = FALSE]
    
    # Compute the linear predictor for stratum s
    # eta_is = x_is^T beta
    eta <- as.vector(Xs %*% beta)
    
    # Convert linear predictors to unnormalized intensities
    # These are proportional to multinomial probabilities
    ps <- exp(eta)
    
    # Normalize to obtain multinomial probabilities
    # p_is = exp(eta_is) / sum_j exp(eta_js)
    # These probabilities sum to 1 within each stratum
    ps <- ps / sum(ps)
    
    # Compute the observed total count in stratum s
    # This is the conditioning value in the conditional Poisson
    Ns <- sum(y[idx])
    
    # Construct the multinomial covariance (weight) matrix
    # diag(ps) gives Var(Y_is | Ns)
    # tcrossprod(ps) = ps %*% t(ps) gives Cov(Y_is, Y_js | Ns)
    #
    # Ws = diag(ps) - ps ps^T
    #
    # This matrix:
    # - encodes negative correlation within strata
    # - has rank (n_s - 1)
    # - removes one degree of freedom due to conditioning
    Ws <- diag(ps) - tcrossprod(ps)
    
    # Add the Fisher information contribution from stratum s
    #
    # Multinomial Fisher information:
    # I_s = Ns * X_s^T Ws X_s
    #
    # Ns scales the information by the total count in the stratum
    I <- I + Ns * crossprod(Xs, Ws %*% Xs)
  }
  
  # Invert the total Fisher information matrix
  # This yields the variance–covariance matrix of beta
  # under the conditional Poisson (multinomial) likelihood
  return(ginv(I))
}
