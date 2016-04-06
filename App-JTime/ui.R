shinyUI(fluidPage(
  titlePanel("Observed Journey Time from Traffic Master Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Choose a link ID from the South West Regional Model
               and visualize the observed journey time on this link
               in the Traffic master dataset."),
      
      textInput("SA_LinkID", 
                label = "Enter Saturn Link ID", 
                value = "10001-10024"),
      
      checkboxGroupInput("Periods", 
                         label = "Select period(s)", 
                         choices = list("Night Time (00:00-07:00)" = "NT", 
                                        "AM Peak (07:00-10:00)" = "AM", 
                                        "Inter Peak (10:00-16:00)" = "IP", 
                                        "PM Peak (16:00-19:00)" = "PM", 
                                        "Evening (19:00-00:00)" = "EVE"),
                         selected = "AM"),
      
      radioButtons("Quantity", 
                   label = "Select quantity to visualize",
                   choices = list("Mean JT on TM Links" = "Mean", 
                                  "Median JT on TM Links" = "Median"),
                   selected = "Mean")
      ),
    
    mainPanel(plotOutput(outputId = "plot", 
                         width = "100%",
                         height = "700px"),
              dataTableOutput("table")
    )
    )
))
