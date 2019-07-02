library(shiny)
library(ggplot2)
set.seed(54321)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

                
  output$histPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
          min <- input$min
          max <- input$max
          trials <- input$trials
          
          Avg <- NULL
          N <- NULL
          
          for (j in 1:trials){
                  x <- mean(sample(min:max,j, replace = TRUE))
                  Avg[j] <- x
                  N[j] <- j
          }
          m <- cbind.data.frame(N,Avg)
          
          h <- hist(Avg)
          
          x <- seq(min(Avg), max(Avg), length = trials)
          y <- dnorm(x, mean = mean(Avg), sd = sd(Avg))
          y <- y * diff(h$mids[1:2])*length(Avg)
          ylim = if(max(y)*1.05>max(h$counts)) {ylim <- max(y)*1.05} else {ylim <- max(h$counts)}
          hist(Avg, ylim = c(0,ylim))
          lines(x, y, col = "red", lwd = 2)
          abline(v = mean(Avg), col = "red", lwd = 2)
          abline(v = (max + min)/2, col = "blue", lwd = 3)
          
  })
  
  output$Plot <- renderPlot({
          
          set.seed(54321)
          min <- input$min
          max <- input$max
          trials <- input$trials
          
          Avg <- NULL
          N <- NULL
          
          for (j in 1:trials){
                  x <- mean(sample(min:max,j, replace = TRUE))
                  Avg[j] <- x
                  N[j] <- j
          }
          m <- cbind.data.frame(N,Avg)
          
          for(i in 1:nrow(m)){
                  m$cent.Avg[i] <- m$Avg[i] - mean(m$Avg)
          }
          
          for (i in 1:nrow(m)){
                  m$Scale.Avg[i] <- m$Avg[i] / sd(m$Avg)
          }
          for (i in 1:nrow(m)){
                  m$Std.Avg[i] <- (m$Avg[i] - mean(m$Avg))/sd(m$Avg)
          }
          
          par(mfrow = c(1,3))
          plot(m$Avg, m$N, main = "Original Data", ylab = "Trial Number", xlab = "Average Value")
          plot(m$cent.Avg, m$N, main = "Centered Data", ylab = "Trial Number", xlab = "Average Value")
          plot(m$Std.Avg, m$N, main = "Standardized Data", ylab = "Trial Number", xlab = "Average Value")
          
  })
  
})
