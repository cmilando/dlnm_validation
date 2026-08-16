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

rtPlotUI <- function(id, title = NULL) {
  
  ns <- NS(id)
  
  tagList(
    
    if (!is.null(title)) {
      h4(title)
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