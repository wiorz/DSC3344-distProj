# Central Limit Theorem Simulation Project
# DSC 3344
# Authors: Daniela Cortes Bermudez, 
#          Ivan Ko, 
#          Claire Teng, 
#          Ty Wicks
# Description: The purpose of the Central Limit Theorem Simulation is to 
# display the normal, exponential, uniform, binomial, chi square, student t, 
# logarithmic, and poisson distributions of an unordered set of objects. 
# The user can use the slider to select an input value, referencing the 
# additional comments as a guide, then running the simulation to update the 
# graphs. Radio buttons provide option for distribution types when can be ran 
# upon selection. 

library(shiny)

# global var
rep <- sample(5000:20000, 1) # Replications is a random integer select from range
# For sampling distribution, use [inf] instead of sample()

# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarPanel(
        # main title
        h2('Central Limit Theorem'),
        
        # instructions to use the simulation
        helpText('Input a value for the parameters, while considering the comments 
              inside "Help". Then, click "Run Simulation" to update
              graphs.'),
        
        # reference
        h6(a("Referenced from: https://github.com/coursephd/week4shinyApp")),
        
        hr(),
        
        actionButton("helpTrigger", "Help"),
        actionButton("resetTrigger", "reset"),
        
        # user enter input for sample size 
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
        
        actionButton("runSim","Run Simulation")
        
    ),
    
    # plotting the distributions
    mainPanel(
        plotOutput("plotOut")
    ) 
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # set random seed to make each run different
    set.seed(runif(1) * as.numeric(Sys.time())) # double randomness!
    
    # Container for the variables needed for simulation.
    container <- reactiveValues(sample = NULL,
                                means = NULL)
    
    # need this to reset values on the UI, otherwise they are read-only
    uiVar <- reactiveValues(n = 1,
                            par1 = 1,
                            par2 = 0
    )
    
    # Settings for helper text
    observeEvent(input$helpTrigger, 
                 {
                   showModal(modalDialog(
                     title = "Usage Guide",
                     p(HTML(
                         "<style>
                           .tab1{tab-size: 2}
                         </style>
                         The parameter that has to be included is specified on 
                         parenthesis next to the distribution name,<br>
                         <pre class='tab1'>Normal(<b>Mean, Standard Deviation</b>)</pre><br>
                         If only one variable is shown, the second parameter is 
                         not used.<br><br>
                         <h3>For Uniform distribution:</h3><br>
                         First parameter must be larger than or equal to second 
                         parameter (Max >= Min).<br>
                         <h3>For Binomial distribution:</h3><br>
                         Second parameter must be the decimal representation of 
                         the probability."
                     )),
                     easyClose = TRUE,
                     footer = NULL
                   ))
                 }
    )
    
    # Reset values on UI
    observeEvent(input$resetTrigger, 
                 {
                   uiVar$n = 1 # this is why we need the input to be reactive
                   uiVar$par1 = 1
                   uiVar$par2 = 0
                   updateTextInput(session, "n", value = uiVar$n)
                   updateTextInput(session, "par1", value = uiVar$par1)
                   updateTextInput(session, "par2", value = uiVar$par2)
                   
                   # reset container values too to trigger plot clear in logic
                   container$sample = NULL
                   container$mean = NULL # mean isn't needed but for compeleteness
                 }, priority = 100
    )
    
    # Using observeEvent() to get values from UI's actionButton()
    # Extract input from UI's output using input$[variable], i.e. input$par1
    observeEvent(input$runSim, {
    
      # create distribution here based on input selection
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
                               # par2 needs to be a percentage in decimal form for B
                               "B" = matrix(rbinom(input$n*rep, 
                                                   input$par1,
                                                   input$par2),
                                            rep),
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
            if (input$runSim) 
            {
                # clear canvas if reset button is clicked
                if(sum(container$sample) == 0)
                {
                  return()
                }
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
