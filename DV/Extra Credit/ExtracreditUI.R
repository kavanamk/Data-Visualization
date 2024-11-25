library(shiny)
library(lubridate)
library(zoo)
library(ggplot2)


portland_wl <- read.csv("~/Desktop/DV/HW2/PortlandWaterLevel2003.csv", stringsAsFactors = TRUE)
portland_wl$DateTime <- as.POSIXct(mdy_hm(paste(portland_wl$Date, portland_wl$Time)))


ui <- fluidPage(
  titlePanel("Portland Water Level - Moving Average"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("days", "Select number of days for moving average:", 
                  min = 1, max = 10, value = 3)
    ),
    
    mainPanel(
      plotOutput("waterLevelPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$waterLevelPlot <- renderPlot({
    # Calculate the moving average based on user input
    hours <- input$days * 24  # Convert days to hours
    portland_wl$MovingAvg <- rollmean(portland_wl$WL, k = hours, fill = NA, align = "right")
    
    # Plot the moving average
    ggplot(portland_wl, aes(x = DateTime, y = MovingAvg)) +
      geom_line(color = "blue", size = 0.5) +
      labs(
        title = paste(input$days, "-Day Moving Average of Water Level"),
        x = "DateTime",
        y = paste(input$days, "-Day Moving Average of WL")
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))  # Center the title
  })
}

# Run the app
shinyApp(ui = ui, server = server)

