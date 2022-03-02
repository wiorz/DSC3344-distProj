###########################################################
## ui
###########################################################

ui <- fluidPage(
  sidebarPanel(
    # main title
    h2('Central Limit Theorem'),
    
    # instructions to use the simulation
    helpText('Input a value for the parameters, while considering the comments 
              under "Additional Comments". Then, click "Run Simulation" to update
              graphs.'),
    
    # reference
    h6(a("Referenced from: https://github.com/coursephd/week4shinyApp")),
  
    # user selection on input size 
    sliderInput(inputId="n","Sample Size n",value=30,min=5,max=100,step=2),
  
    # parameters 
    numericInput("param1","First Parameter",10),
    numericInput("param2","Second Parameter",1),
    
    # selection of distribution type
    radioButtons("src.dist", "Distribution type:",
                  c("Exponential (Mean)" = "E",
                    "Normal (Mean, SD)" = "N",
                    "Uniform (Min, Max)" = "U",
                    "Logarithmic (Mean, Standard Deviation)" = "L",
                    "Poisson (Lambda)" = "P",
                    "Binomial (Size, Success Probability)" = "B",
                    "Chi Square (DF, ncp)" = "X", 
                    "Student t (DF)" = "T")),
    
    actionButton("takeSample","Run Simulation"),
    
    # additional instrucitions on how to run the simulation
    h5('Additional Comments'),
    helpText('The parameter that has to be included is specified on parenthesis next to 
              the distribution name. If only one variable is included, do not input
              the second parameter'),
    h6('For Uniform distribution:'),
    helpText('Param1 must be larger than Param2.'),
    h6('For Binomial distribution:'),
    helpText('Param2 must be the decimal representation of the probability.'),
    h6('For Logarithmic distribution:'),
    helpText('Param2 must be the decimal representation of st. deviation.')
  ),

  # plotting the distributions
  mainPanel(
    plotOutput("plotSample")
  ) 
) 
