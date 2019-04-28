#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)

my_header <- dashboardHeader(
    title = "Outpatients explorer"
)

my_sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            "Demographics",
            tabName = "demo"
            ),
        menuItem(
            "By locality",
            tabName = "by_locality"
        ),
        menuItem(
            "By clinic",
            tabName = "by_clinic"
        ),
        menuItem(
            "Data",
            tabName = "Data"
        )
        
    )
)

my_body <- dashboardBody()


# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "green",
    header <- my_header,
    sidebar <- my_sidebar,
    body <- my_body
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
