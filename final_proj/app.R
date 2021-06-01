#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(maps)

# load data
shoot <- read.csv("fatal-police-shootings-data.csv")
## provide reference to this data 
fips_data <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")

state_abb_data <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
state_abb_data$State <- tolower(state_abb_data$State)

# manipulate data 
shoot_map_data <- shoot %>% 
    select(id, armed, state, signs_of_mental_illness)
# add fips data to shooting data 
fips_data <- fips_data %>% select(state_abbr, fips)
shoot_map_data <- left_join(shoot_map_data, fips_data, by = c("state" = "state_abbr"))



# create state data 
state_shapes <- map_data("state")
state_shapes <- left_join(state_shapes, state_abb_data, by = c("region" = "State"))

# attach state data to shooting data 
shoot_map <- left_join(state_shapes, shoot_map_data, by = c("Abbreviation" = "state"))

# plot map 
shoot_map_plot <- ggplot(shoot_map, aes(long, lat, group = group)) +
    geom_polygon() + coord_quickmap()



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
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
