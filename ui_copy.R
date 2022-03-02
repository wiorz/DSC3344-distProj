###########################################################
## ui
###########################################################

ui <- fluidPage(
  titlePanel('Sampling Distributions and the Central Limit Theorem'),
  sidebarPanel(
    helpText('Choose your source distribution and number of items, n, in each
             sample. 10000 replications will be run when you click "Sample Now".'),

    h6(a("Read an article about this simulation at https://www.r-bloggers.com",
         href="https://www.r-bloggers.com/sampling-distributions-and-central-limit-theorem-in-r/", target="_blank")),

    sliderInput(inputId="n","Sample Size n",value=30,min=5,max=100,step=2),
    
    numericInput("param1","Parameter 1:",1),
    numericInput("param2","Parameter 2:",0),

    radioButtons("src.dist", "Distribution type:",
                 c("Exponential: Param1 = Mean,     Param2 = NOT USED" = "E",
                   "Normal:      Param1 = Mean,     Param2 = SD" = "N",
                   "Uniform:     Param1 = Min,      Param2 = Max" = "U",
                   "Poisson:     Param1 = Lambda,   Param2 = NOT USED" = "P",
                   "Loganormal:  Param1 = meanlog,  Param2 = sdlog" = "L",
                   "Binomial:    Param1 = Size,     Param2 = Success prob" = "B",
                   "Gamma:       Param1 = Shape,    Param2 = Scale" = "G",
                   "Chi Square:  Param1 = DF,       Param2 = ncp" = "X",
                   "Student t:   Param1 = DF,       Param2 = NOT USED" = "T")),

    
    actionButton("takeSample","Run Simulation")
    ),

  # end sidebarPanel

  mainPanel(
    # Use CSS to control the background color of the entire page
    tags$head(
      tags$style("body {background-color: #9999aa; }")
    ),

    plotOutput("plotSample")
  ) # end mainPanel
) # end UI
