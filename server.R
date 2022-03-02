library(shiny)

# global var
rep <- sample(5000:20000, 1) # Replications is a random integer select from range
# for sampling distribution, use [inf] instead of sample()

dynamic <- function(input, output) {
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
                                                 input$par1,
                                                 input$par2),
                                          rep),
                              "B" = matrix(rbinom(input$n*rep, 
                                                  input$par1,
                                                  input$par2),
                                          rep),
                              # par2 needs to be a percentage in decimal form for binom
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
          par(mfrow=c(2,1), mar=c(3,1,5,1), oma=c(0,1,3,0)) #TODO: play with this to make it look pretty!
          hist(container$sample,    
               main="Distribution of Samples",            
               ylab="Frequency", 
               col= "aquamarine4")
          hist(container$means, 
               main="Sampling Distribution of the Mean",  
               ylab="Frequency", 
               col="goldenrod3")
    
          # Title text setting
          mtext("Simulation Results", outer=TRUE, cex=3.5, font = 2)
        }
      #TODO: change aspect ratio?
    }, height=560, width=700) # aspect ration of 5:4! 

}
