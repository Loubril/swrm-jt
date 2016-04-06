

# Load packages
suppressPackageStartupMessages(library("shiny"))
suppressPackageStartupMessages(library("ggplot2"))

# Import data
jts <- read.csv(file = "Data/Processed_JT.csv",
                sep = ",", 
                header = TRUE)

# Load functions
source("jts_plots.r")


shinyServer(function(input, output) {
  
  # Reactive function depending only on SA_LinkID, returning table for input Link ID
  jts_link <- reactive({
    f_GetLinkData(jts,input$SA_LinkID)
  })
  
  # Output 1: Plot
  output$plot <- renderPlot({
    f_GetPlot(jts_link(),input$SA_LinkID,input$Periods,input$Quantity)
  })
  
  # Output 2: table
  output$table <- renderDataTable({
    f_GetTable(jts_link(),input$SA_LinkID,input$Periods,input$Quantity)
  }) 
  
})