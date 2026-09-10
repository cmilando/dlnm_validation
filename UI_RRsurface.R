RRsurfacePanel <- tabPanel(
  "RR surface",
  
  helpText("This page allows the user to defined the RR surface visually.
               The spline degree can change how linear the surfaces interact.
               The lag graph determines how the surface behaves in the lag 
               dimension. This is not fully bulletproof,
               there are likely some edge cases that will break how this 
               works."),
  
  shiny::hr(),
  
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
                     exposure value.", islag = T)
      )
    )
    
  ),
  
  shiny::hr(),
  
  ##
  combinedRRPlotUI(
    "combined1",
    "RR Surface",
    w2 = 8, w3 = 2
  )
  
  ##
  
)