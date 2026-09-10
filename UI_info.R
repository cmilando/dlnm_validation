infoPanel <- tabPanel(
  
  "Info",
  
  fluidRow(
    
    column(
      width = 10,
      offset = 1,
      
      div(
        class = "plot-container",
        style = " padding: 30px 40px;
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
)