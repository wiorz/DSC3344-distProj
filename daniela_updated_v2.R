# Central Limit Theorem Simulation Project
# DSC 3344
# Daniela Cortes Bermudez, Ivan Ko, Claire Teng, Ty Wicks

library(shiny)

# Define UI for application that draws a histogram
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
        # user selection on input size 
        numericInput("n","Input Sample Size (Integer)", 1),
        
        # parameters 
        numericInput("par1","First Parameter", 1),
        numericInput("par2","Second Parameter", 0),
        
        # selection of distribution type
        radioButtons("src.dist", "Select Distribution:",
                     c("Normal (Mean, Standard Deviation)" = "N",
                       "Exponential (Mean)" = "E",
                       "Uniform (Max, Min)" = "U",
                       "Binomial (Size, Success Probability)" = "B",
                       "Chi Square (Degree of Freedom,  Non-centrality Parameter)" = "X", 
                       "Student t (Degree of Freedom)" = "T",
                       "Logarithmic (Mean, Standard Deviation)" = "L",
                       "Poisson (Lambda)" = "P")),
        
        actionButton("uiOutput","Run Simulation"),
        
        # additional instructions on how to run the simulation
        h5('Additional Comments'),
        helpText('The parameter that has to be included is specified on parenthesis next to 
              the distribution name. If only one variable is included, do not input
              the second parameter'),
        h6('For Uniform distribution:'),
        helpText('First parameter must be larger than second parameter.'),
        h6('For Binomial distribution:'),
        helpText('Second parameter must be the decimal representation of the probability.'),
        h6('For Logarithmic distribution:'),
        helpText('Second parameter must be the decimal representation of st. deviation.')
    ),
    
    # plotting the distributions
    mainPanel(
        plotOutput("plotOut")
    ) 
)

# global var
rep <- sample(5000:20000, 1) # Replications is a random integer select from range
# for sampling distribution, use [inf] instead of sample()

# Define server logic required to draw a histogram
server <- function(input, output) {
    # set random seed to make each run different
    set.seed(runif(1) * as.numeric(Sys.time())) # double randomness!
    
    # Container for the variables needed for simulation.
    container <- reactiveValues(sample = NULL,
                                means = NULL)
    
    # Using observeEvent() to get values from UI's actionButton()
    # Extract input from UI's output using input$[variable], i.e. input$par1
    observeEvent(input$uiOutput, {
        
        sampleInstance <- switch(input$src.dist,
                                 # Each label is a matrix of size rep, filled with 
                                 # random distribution of the selected input from UI
                                 "N" = matrix(rnorm(input$n*rep, 
                                                    input$par1,
                                                    input$par2),
                                              rep),
                                 "E" = matrix(rexp(input$n*rep, 
                                                   input$par1),
                                              rep),
                                 "U" = matrix(runif(input$n*rep,
                                                    input$par2,
                                                    input$par1),
                                              rep),
                                 "B" = matrix(rbinom(input$n*rep, 
                                                     input$par1,
                                                     input$par2),
                                              rep),
                                 # par2 needs to be a percentage in decimal form
                                 "X" = matrix(rchisq(input$n*rep,
                                                     input$par1),
                                              rep),
                                 "T" = matrix(rt(input$n*rep,
                                                 input$par1),
                                              rep),
                                 "L" = matrix(rlnorm(input$n*rep,
                                                     input$par1,
                                                     input$par2),
                                              rep),
                                 "P" = matrix(rpois(input$n*rep, 
                                                    input$par1)
                                              ,rep))
        # plotting container requires numeric values!
        container$sample <- as.numeric(sampleInstance[1,])
        container$means <- as.numeric(apply(sampleInstance,1,mean))
    })
    
    # Plot is triggered by actionButton(), which is when user click "Run Simulation"
    output$plotOut <- renderPlot(
        {
            if (input$uiOutput) 
            {
                # Plot graphs in form of 2 rows with one graph per row.  
                # mar and oma for space control
                par(mfrow=c(2,1), oma=c(0,0,2,0))
                hist(container$sample,    
                     main="Distribution of Samples",            
                     ylab="Frequency", 
                     col= "aquamarine4",
                     xlab = "Sample")
                hist(container$means, 
                     main="Sampling Distribution of the Mean",  
                     ylab="Frequency", 
                     col="goldenrod3",
                     xlab="Means")
            }
            
        }, height=855, width=800)  
}

# Run the application 
shinyApp(ui = ui, server = server)
