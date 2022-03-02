
ui <- fluidPage(
  sidebarPanel(
    # main title
    h2('Central Limit Theorem'),
    
    # instructions to use the simulation
    helpText('Input a value for the sample size and then parameters, while 
              considering the comments under "Additional Comments". Then, click 
              "Run Simulation" to update graphs.'),
    
    # reference
    h6(a("Referenced from: https://github.com/coursephd/week4shinyApp")),
    
    # user selection on input size 
    numericInput("n","Input Sample Size (Integer)", 1),
    
    # parameters 
    numericInput("par1","First Parameter", 1),
    numericInput("par2","Second Parameter", 0),
    
    # selection of distribution type
    radioButtons("src.dist", "Select Distribution:",
                 c("Normal (Mean, SD)" = "N",
                   "Exponential (Mean)" = "E",
                   "Uniform (Max, Min)" = "U",
                   "Binomial (Size, Success Probability)" = "B",
                   "Chi Square (Degree of Freedom,  non-centrality parameter)" = "X", 
                   "Student t (Degree of Freedom)" = "T",
                   "Logarithmic (Mean, Standard Deviation)" = "L",
                   "Poisson (Lambda)" = "P")),
    
    actionButton("uiOutput","Run Simulation"),
    
    # additional instrucitions on how to run the simulation
    h5('Additional Comments'),
    helpText('The parameter that has to be included is specified on parenthesis next to 
              the distribution name. If only one variable is included, do not input
              the second parameter'),
    h6('For Uniform distribution:'),
    helpText('Parameter 2 must be larger than Parameter 1.'),
    h6('For Binomial distribution:'),
    helpText('Parameter2 must be the decimal representation of the probability.')
  ),
  
  # plotting the distributions
  mainPanel(
    plotOutput("plotOut")
  ) 
) 
