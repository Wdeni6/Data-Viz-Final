library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

dat <- read_csv("~/MASTERS/DATA 824 Data Viz/Module 12/Amazon_data.csv")
dat$Weatherconditions <- as.factor(dat$Weatherconditions)
dat$Road_traffic_density <- as.factor(dat$Road_traffic_density)
dat$Vehicle_condition <- as.factor(dat$Vehicle_condition)
dat$Type_of_order <- as.factor(dat$Type_of_order)
dat$Type_of_vehicle <- as.factor(dat$Type_of_vehicle)
dat$multiple_deliveries <- as.factor(dat$multiple_deliveries)
dat$Festival <- as.factor(dat$Festival)
dat$City <- as.factor(dat$City)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("lumen"),
  # Application title
  titlePanel("Final Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins",
                  "Time Taken (Number of Histogram bins):",
                  min = 1,
                  max = 50,
                  value = 30),
      
      # # Select type of trend to plot
      selectInput(inputId = "selectCcolumn", label = strong("Numeric Field Name"),
                  choices = names(dat[,4:5]),
                  selected = dat$Delivery_person_Ratings),
      
      selectInput(inputId = "selectDcolumn", label = strong("Categorical Field Name"),
                  choices = names(dat[,6:13]),
                  selected = dat$Weatherconditions),
      
      # Select whether to overlay smooth trend line
      checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
   
      checkboxInput(inputId = "Summ", label = strong("Display header summary of data"), value = TRUE)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("ScatterPlot"),
      plotOutput("BoxPlot"),
      tableOutput("Header")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- dat[, 14]
    bins <- seq(min(x$Time_taken), max(x$Time_taken), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x$Time_taken, breaks = bins, col = 'darkgray', main="Histogram of Time Taken", border = 'white', xlab = "Time Taken")
  })
  
  output$ScatterPlot <- renderPlot({
    
    if (input$smoother) {
      ggplot(data=dat, aes_string(x=input$selectCcolumn, y=dat$Time_taken)) +
        geom_jitter() +
        scale_x_continuous() +
        geom_smooth() +
        ylab("Time Taken")
    } else {
      ggplot(data=dat, aes_string(x=input$selectCcolumn, y=dat$Time_taken)) +
        geom_jitter() +
        scale_x_continuous() +
        ylab("Time Taken")
    }
  })
  
  
  output$BoxPlot <- renderPlot({
    ggplot(data=dat, aes_string(x=input$selectDcolumn, y=dat$Time_taken)) +
      geom_boxplot() +
      scale_x_discrete() +
      ylab("Time Taken")
  })
  
  output$Header <- renderTable({
    if (input$Summ) {
      head(dat, n= 10L)
    } else {
      " "
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
