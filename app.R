library(shiny)
library(dplyr)
library(shinydashboard)
library(gridExtra)
library(grid)
library(ggplot2)

source("data/filter.R")

dayofweek_options <- c("No Filter", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
length_options <- c("No Filter", "Short Flight", "Medium Flight", "Long Flight")

ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Filters"),
    dashboardSidebar( width = 160,
                      sidebarMenu(style = "position: fixed; width:150px;",
                                  selectInput("gender", 
                                              label = "Day of Week:",
                                              choices = c(dayofweek_options), 
                                              selected = "No Filter"),
                                  selectInput("senior", 
                                              label = "Flight Length:",
                                              choices = c(length_options), 
                                              selected = "No Filter"),
                      )

    ),
    
    dashboardBody(
      fluidrow(
        box(title = "Percentage of Delay Flights", width = NULL, solidHeader = TRUE, status = "primary", color = "#286192",
            fluidRow(
              box(align = "center",
                  title = NULL, 
                  width = 12,
                  status = NULL,
                  box(width = NULL, plotOutput("plot1", height = "150px")))
            ))),
      fluidRow(
        box(title = "Percentage of Airline Delayed", width = NULL, solidHeader = TRUE, status = "primary", color = "#286192",
            fluidRow(
              box(align = "center",
                  title = NULL, 
                  width = 12,
                  status = NULL,
                  box(width = NULL, plotOutput("plot1", height = "150px")))
            ))), 
            fluidRow(
              column(width = 6,
                     box(title = "Departure Airport Delayed Top 10", width = NULL, solidHeader = TRUE, status = "primary",
                     )),  
              column(width = 6,
                     box(title = "Arrival Airport Delayed Top 10", width = NULL, solidHeader = TRUE, status = "primary",
                     ))
              )
      )
    shinyApp(ui, server)

        
              
              
              
              
              
