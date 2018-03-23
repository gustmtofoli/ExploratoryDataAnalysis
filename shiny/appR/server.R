#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    data_frame <- data.frame(A = c(15, 16, 17, 18, 19, 20, 21, 22, 23),
                             B = c(10, 20, 30, 40, 80, 50, 60, 70, 15))
    
    # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'blue', border = 'white')
    ggplot(data = data_frame, aes(x = A, y = B)) +
      geom_line(color = 'blue') +
      xlab("age") +
      ylab("number of users")
    
  })
  
})
