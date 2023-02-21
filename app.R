library(shiny)
library(dplyr)
library(ggplot2)

# Load data
dataset <- read.csv('airlines_delay.csv')

# Set labels for Class variable
dataset$Class[dataset$Class == 1] <- 'Delayed'
dataset$Class[dataset$Class == 0] <- 'On_Time'

# Convert DayOfWeek to factor with appropriate labels
dataset$DayOfWeek <- factor(dataset$DayOfWeek, levels=1:7, labels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Convert Length to factor with appropriate levels and labels
dataset$Length <- factor(ifelse(dataset$Length %in% 0:180, "Short Flight",
                                ifelse(dataset$Length %in% 181:360, "Medium Flight", "Long Flight")),
                         levels = c("Short Flight", "Medium Flight", "Long Flight"))

dayofweek_options <- c("No Filter", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
length_options <- c("No Filter", "Short Flight", "Medium Flight", "Long Flight")

# Define UI
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Filters"),
    dashboardSidebar(width = 160,
                     sidebarMenu(style = "position: fixed; width: 150px;",
                        selectInput("day", 
                                    "Day:",
                                    choices = c(dayofweek_options),
                                    selected = "No Filter"),
                        selectInput("length", 
                                    "Length:",
                                    choices = c(length_options),
                                    selected = "No Filter")
                      )
    ),
    
    dashboardBody(
      # Display plot 1 - on-time and delayed flights percentage
      plotOutput("plot1", height = "400px"),
      
      # Display plot 2 - delayed percentage of each airline
      plotOutput("plot2", height = "400px"),
      
      # Display plot 3 - top 10 departure and arrival airports with most delayed flights
      fluidRow(
        column(6, plotOutput("plot3a", height = "400px")),
        column(6, plotOutput("plot3b", height = "400px"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create reactive values for dataset and filtered data
  rv = reactiveValues()
  rv$dataset = dataset
  
  # Filter data based on user inputs
  observe({
    rv$filteredData = rv$dataset %>%
      filter(if(input$day == 'No Filter'){DayOfWeek %in% dayofweek_options} else {DayOfWeek == input$day}) %>%
      filter(if(input$length == 'No Filter'){Length %in% length_options} else {Length == input$length})
  })
  
  # Display plot 1 - on-time and delayed flights percentage
  output$plot1 <- renderPlot({
    # Calculate on-time and delayed percentages
    onTimePct <- round(100 * nrow(filter(rv$filteredData, Class == 'On_Time')) / nrow(rv$filteredData))
    delayedPct <- round(100 * nrow(filter(rv$filteredData, Class == 'Delayed')) / nrow(rv$filteredData))
    
    # Create bar chart of on-time and delayed percentages
    plot1 <- ggplot(data = data.frame(Class = c("On_Time", "Delayed"), Percentage = c(onTimePct, delayedPct))) +
      geom_col(aes(x = Class, y = Percentage, fill = Class)) +
      coord_flip() +
      scale_fill_manual(values = c("#B71C1C", "#5CB85C")) +
      xlab("Flight Status") +
      ylab("Percentage") +
      ggtitle("On-Time and Delayed Flights Percentage") +
      theme(legend.position = "none")
    plot1
  })
  
  output$plot2 <- renderPlot({
    
    # Calculate delayed percentages by airline
    Airline_delay <- rv$filteredData %>%
      group_by(Airline) %>%
      summarize(Delayed = sum(Class == "Delayed"), Total = n()) %>%
      mutate(Per_delayed = round(100 * Delayed / Total, 2)) %>%
      arrange(desc(Per_delayed))
    
    # Create bar chart of delayed percentages by airline
    plot2 <- ggplot(data = Airline_delay) +
      geom_col(aes(x = reorder(Airline, Per_delayed), y = Per_delayed, fill = Airline)) +
      scale_fill_viridis_d(option = "plasma") +
      xlab("Airline") +
      ylab("Delayed Percentage") +
      ggtitle("Delayed Percentage by Airline") +
      theme(legend.position = "none") +
      ylim(0,100)
    plot2
  })
  
  output$plot3a <- renderPlot({
    # Calculate top 10 departure airports with most delayed flights
    topDepartureAirports <- rv$filteredData %>%
      filter(Class == 'Delayed') %>%
      group_by(AirportFrom) %>%
      summarize(numDelayed = n()) %>%
      arrange(desc(numDelayed)) %>%
      head(10)
    
    # Create bar chart of top 10 departure airports with most delayed flights
    ggplot(data = topDepartureAirports) +
      geom_col(aes(x = reorder(AirportFrom, numDelayed), y = numDelayed, fill = AirportFrom)) +
      scale_fill_viridis_d(option = "plasma") +
      xlab("Departure Airport") +
      ylab("Number of Delayed Flights") +
      ggtitle("Top 10 Departure Airports with Most Delayed Flights") +
      theme(legend.position = "none")
  })
  
  output$plot3b <- renderPlot({
    # Calculate top 10 arrival airports with most delayed flights
    topArrivalAirports <- rv$filteredData %>%
      filter(Class == 'Delayed') %>%
      group_by(AirportTo) %>%
      summarize(numDelayed = n()) %>%
      arrange(desc(numDelayed)) %>%
      head(10)
    
    # Create bar chart of top 10 arrival airports with most delayed flights
    plot3 <- ggplot(data = topArrivalAirports) +
      geom_col(aes(x = reorder(AirportTo, numDelayed), y = numDelayed, fill = AirportTo)) +
      scale_fill_viridis_d(option = "plasma") +
      xlab("Arrival Airport") +
      ylab("Number of Delayed Flights") +
      ggtitle("Top 10 Arrival Airports with Most Delayed Flights") +
      theme(legend.position = "none")
    plot3
  })
  
}

shinyApp(ui = ui, server = server)

