function(input, output) {
  
  load(file = "logfit.Rdata")
  
  
  output$logfit<- renderPlot({
    plot.list[[input$models]] + geom_vline(xintercept=input$threshold, linetype = "dashed",
                                           color = "black")
  })
  
  output$threshold <- renderPlot({
    confusion.matrix(table(ifelse(as.numeric(threshold.list[[input$models]] > input$threshold), 1, 0), test.data$y),
                     tit = paste0(input$models, " classification probability threshold ", input$threshold * 100, "%"))
    
  })
}