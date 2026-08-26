downloadPanel <- tabPanel(
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
        ),
        
        br(), br(),
        
        p("Sample code:"),
        
        br(),
        
        verbatimTextOutput("codeSnip")
        
      )
    )
    
  )
)