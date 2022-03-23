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
library(shinyWidgets)

# global var
SLEEPTIME = 300
DEBUG = TRUE

# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarPanel(
        # main title
        h2('Central Limit Theorem'),
        
        # instructions to use the simulation
        helpText('Input a value for the parameters, while considering the comments 
              inside "Help". Then, click "Run Simulation Once" to update
              graphs. Or use the Start and Stop button to control animation.'),
        
        # reference
        h6(a("Referenced from: https://github.com/coursephd/week4shinyApp")),
        
        actionButton("helpTrigger", "Help"),
        
        # user enter input for population and sample size, and repetition
        numericInput("n","Input Population Size (Integer)", 100, min = 0),
        numericInput("sam","Sample Draw Size (Integer)", 10, min = 1),
        numericInput("rep", "Repetition of Drawing Samples from Same Population", 1, min = 1),
        
        # parameters 
        numericInput("par1","First Parameter", 1),
        numericInput("par2","Second Parameter", 0, min = 0),

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
        
        actionBttn("runSim","Run Simulation Once", style = "float", color = "primary"),
        
        h3(" - Animation - "),
        actionBttn("start", "Start", style = "jelly", color = "success"),
        actionBttn("stop", "Stop", style = "unite", color = "danger"),
        actionBttn("resetTrigger", "Reset", style = "jelly", color = "warning"),
        h4("Warning: Stop before using Reset!")
        
    ),
    
    # plotting the distributions
    mainPanel(
        plotOutput("plotOut"),
        
        # visual data on same row
        fluidRow(
          textOutput("sampleCount")
        )
    ) 
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # set random seed to make each run different
    set.seed(runif(1) * as.numeric(Sys.time())) # double randomness!
  
    # function for making distribution 
    # distribution switch, do it once to make population, then only change 
    # samples! Reset changes the population
    getPop <- function()
    {
      # input validation here to avoid errors
      if(input$n <= 0){
        validate("Population must be at least 1!")
      }
      if(input$sam <= 0){
        validate("Sample must be at least 1!")
      }
      if(input$rep <= 0){
        validate("Repetition must be at least 1!")
      }
      if(input$par2 < 0){
        validate("Standard Deviation must be positive!")
      }
      
      population <- switch(input$src.dist,
                           # Each label is a matrix of size rep, filled with 
                           # random distribution of the selected input from UI
                           "N" = rnorm(input$n, input$par1, input$par2),
                           "E" = rexp(input$n, input$par1),
                           "U" = runif(input$n, input$par2, input$par1),
                           # par2 needs to be a percentage in decimal form for B
                           "B" = rbinom(input$n, input$par1, input$par2),
                           "X" = rchisq(input$n, input$par1),
                           "T" = rt(input$n, input$par1),
                           "L" = rlnorm(input$n, input$par1, input$par2),
                           "P" = rpois(input$n, input$par1),
      )
      return(population)
    }
    
    # input: n = sample size, par1 = parameter 1, par2 = parameter 2
    sampleChange <- function()
    {
      # plotting container requires numeric values!
      curSam <- as.numeric(sample(container$pop, input$sam))
      container$samples <- append(container$samples, 
                                 curSam)
      container$means <- append(container$means,
                                as.numeric(mean(curSam)))
      if(DEBUG)
      {
        sprintf("CurStep: %d", container$curStep)
        print("sample:")
        print(container$samples)
        print("mean:")
        print(container$means) 
      }
    }
    
    # Container for the variables needed for simulation.
    container <- reactiveValues(pop = NULL,
                                samples = NULL,
                                means = NULL,
                                curStep = 0
                                )
    
    # need this to reset values on the UI, otherwise they are read-only
    uiVar <- reactiveValues(n = 100,
                            par1 = 1,
                            par2 = 0,
                            sam = 10,
                            rep = 1
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
                   uiVar$n = 100 # this is why we need the input to be reactive
                   uiVar$par1 = 1
                   uiVar$par2 = 0
                   uiVar$rep = 1
                   uiVar$sam = 10
                   updateTextInput(session, "n", value = uiVar$n)
                   updateTextInput(session, "par1", value = uiVar$par1)
                   updateTextInput(session, "par2", value = uiVar$par2)
                   updateTextInput(session, "sam", value = uiVar$sam)
                   updateTextInput(session, "rep", value = uiVar$rep)
                   
                   
                   # reset container values too to trigger plot clear in logic
                   container$pop = NULL
                   container$samples = NULL
                   container$means = NULL 
                   container$curStep = 0
                 }, priority = 100
    )
    
    # Using observeEvent() to get values from UI's actionButton()
    # Extract input from UI's output using input$[variable], i.e. input$par1
    observeEvent(input$runSim, 
                 {
                   container$pop <- getPop()
                   sampleChange()
                   container$curStep <- container$curStep + 1
                   if(DEBUG)
                   {
                     print("curStep in sim = ")
                     print(container$curStep)
                   }
                   
                 }
      )
    
    # use to loop the plot by changing variables in multiple simulations
    anime <- function(timer)
    {
      # check for data existence first
      req(input$rep)
      req(input$sam)
      req(input$par1)
      req(input$par2)
      
      # This is KEY! Allows us to stop the animation once it reaches target value
      # a bug/feature is that it allows us to dynamically update the graph since  
      # it's still istening on the renderPlot(), since input$start is one of the 
      # triggers
      # req(container$curStep < input$rep)
      
      # remake and recalculate variables
      sampleChange()
      
      container$curStep = container$curStep + 1
      
      # if(container$curStep >= input$rep)
      # {
      #   timer<-reactiveTimer(Inf)
      #   return()
      # }
   
      
    }
    
    # init for animation
    counter <- reactiveValues()
    counter$timer <- reactiveTimer(Inf)
    
    # do animation here
    observeEvent(input$start,{
      counter$timer<-reactiveTimer(SLEEPTIME)
      container$pop <- getPop()
      
      if(container$curStep < input$rep)
      {
        observeEvent(counter$timer(),{
          anime()
        })
      }
      
    })
    
    observeEvent(input$stop,{
      counter$timer<-reactiveTimer(Inf)
    })
    
    # ----------------------------------
    # ----------output------------------
    # ----------------------------------
    
    # Plot is triggered by actionButton(), which is when user click "Run Simulation"
    output$plotOut <- renderPlot(
        {
            if (input$runSim || input$start) 
            {
                # clear canvas if reset button is clicked
                if(sum(container$samples) == 0)
                {
                  return()
                }
                # Plot graphs in form of 2 rows with one graph per row.  
                # mar and oma for space control
                par(mfrow=c(3,1), oma=c(0,0,2,0))
              
                
                hist(container$pop,    
                     main="Population Distribution",            
                     ylab="Frequency", 
                     col= "grey",
                     xlab = "Population")
                
                hist(container$samples,    
                     main="Distribution of Samples",            
                     ylab="Frequency", 
                     col= "aquamarine4",
                     xlab = "Samples")
                
                hist(container$means, 
                     main="Sampling Distribution of the Mean",  
                     ylab="Frequency", 
                     col="goldenrod3",
                     xlab="Means")
                # add mean line
                meanMean = mean(container$means)
                abline(v = meanMean,
                       col = "red",
                       lwd = 3)
                # add mean text
                text(x = meanMean * 1.5,
                     y = meanMean * 1.5,
                     paste("Mean =", meanMean),
                     col = "red",
                     cex = 2)
                # add mean density curve, only draw when more then 2 reps!
                if(container$curStep >= 2){
                  lines(density(container$means), col = "blue", lwd = 2)
                }
            }
            
        }, height=855, width=800)
    
    # Update dynamic numeric variable
    output$sampleCount<-renderText({
      paste("\nCurrent Step:", container$curStep)
      paste("\nCurrent Samples Count:", length(container$samples))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
