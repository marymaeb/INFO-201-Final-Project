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

data <- read.csv("fatal-police-shootings-data.csv")
by_race <- data %>%
    group_by(race, gender) %>%
    summarise(shootings = n()) 

# create map of shooting counts 
## load data
fips_data <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")
state_abb_data <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
state_abb_data$State <- tolower(state_abb_data$State)

## manipulate data 
shoot_map_data <- data %>% 
    select(id, armed, state, signs_of_mental_illness)
## create counts of shootings by state 
shoot_count <- shoot_map_data %>% 
    group_by(Abbreviation) %>% 
    summarize(count = n())

## add fips data to shooting data 
fips_data <- fips_data %>% select(state_abbr, fips)
shoot_map_data <- left_join(shoot_map_data, fips_data, by = c("state" = "state_abbr"))

## add fips data to shooting data 
fips_data <- fips_data %>% select(state_abbr, fips)
shoot_map_data <- left_join(shoot_map_data, fips_data, by = c("state" = "state_abbr"))

## create state data 
state_shapes <- map_data("state")
state_shapes <- left_join(state_shapes, state_abb_data, by = c("region" = "State"))

## attach state data to shooting data 
shoot_map <- left_join(state_shapes, shoot_map_data, by = c("Abbreviation" = "state"))
## attach counts to shooting and map data 
shoot_map <- left_join(shoot_map, shoot_count, by = "Abbreviation")

## plot map 
shoot_map_plot <- ggplot(shoot_map, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = count)) + coord_quickmap() + 
    labs(
        title = "Count of Fatal Police Shootings by US State"
    )
    
shoot_map_plot

server <- function(input, output) {
    gender_name <- reactive({
        if(is.null(input$gender)) {
            by_race
        }  else {
            by_race %>%
                filter(gender %in% input$gender) 
        }
    }) 
    output$race_bar <- renderPlot({
        ggplot(gender_name(), aes(race, shootings )) +
            geom_col(col = "Black", fill = input$Color) +
            labs (
                x = "Race", 
                y = "Number of Shootings", 
                title = "Number of Shootings by Race and Gender") +
            theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5))
    })
    output$message <- renderText ({
        high_race <- by_race %>%
            filter(gender == input$gender) %>%
            arrange(desc(shootings))
        paste0(high_race$race[1] , input$gender , " have the highest amount of fatal police shootings with ", high_race$shootings[1], " shootings. ")
    })
    

    
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Police Shootings in the USA Data"),
    
    sidebarLayout(
        sidebarPanel(
            uiOutput("gender"), 
            selectInput(inputId = "gender", "Gender:", 
                        c("Female" = "F", 
                          "Male" = "M")), 
            radioButtons(inputId = "Color", label = "Plot Color", 
                         c("Red", "Blue", "Gray", "Black"), 
                         selected = "Black"
            )
        ),
        mainPanel(
            plotOutput("race_bar"), 
            textOutput("message")
            
        )
    )
)

# Run the application 
shinyApp(ui = ui, server = server)
