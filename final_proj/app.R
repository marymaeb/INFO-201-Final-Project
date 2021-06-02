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


library(shiny)

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





# Bar Graph for the "Manner of Death"
library(shiny)
library(ggplot2)
library(dplyr)


by_manner <- data %>%
    group_by(manner_of_death, gender) %>%
    summarise(occurance = n())




# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Manner of Death"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('var',
                        label= "Select Gender",
                        choices = list("Female"= "FM",
                                       "Male"= "M"))),
        radioButtons(inputId = "Color", label = "Plot Color", 
                     c("Red"), 
                     selected = "Red"),
                       
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("mannerBar")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) 
    gender_name <- reactive({
        if(is.null(input$gender)) {
            by_race
        }  else {
            by_race %>%
                filter(gender %in% input$gender) }
        
        })
  
     output$mannerBar <- renderPlot ({
         ggplot(gender_name, aes(manner_of_death, ocurrances))+ 
             geom_col(col= "Red", fill = input$Color) +
             labs(
                 x= "Mannner Of Death",
                 y= "Ocurrances",
                 title= "Bar Graph of Manner of Death"
             )
     })

# Run the application 
shinyApp(ui = ui, server = server)



