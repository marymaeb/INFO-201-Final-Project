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
library(maps)

# load data
shoot <- read.csv("fatal-police-shootings-data.csv")
## provide reference to this data 
fips_data <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")

# manipulate data 
shoot_map <- shoot %>% select(id, armed, state, signs_of_mental_illness)
fips_data <- fips_data %>% select(state_abbr, fips)

server <- function(input, output) {
    data <- read.csv("fatal-police-shootings-data.csv") 
    
    
    by_race <- data %>%
        group_by(race, gender) %>%
        summarise(shootings = n()) 
    
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
