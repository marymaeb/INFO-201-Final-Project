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

##find highest count by race
  by_race_high <-  by_race %>%
        arrange(desc(shootings))

# create map of shooting counts 
## load data
fips_data <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")
state_abb_data <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")
state_abb_data$State <- tolower(state_abb_data$State)

## manipulate data 
shoot_map_data <- data %>% 
    select(id, armed, state, signs_of_mental_illness)

##find highest shoot count 
shoot_count_highest <- shoot_count %>%
    arrange(desc(count))

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
shoot_map <- shoot_map %>% 
    rename(
        state_count = count
    )
## attach armed counts to shooting map 
shoot_map <- left_join(shoot_map, armed_count, by = "armed")
shoot_map <- shoot_map %>% 
    rename(
        armed_count = count
    )

prop_ill_true <- shoot_map %>% 
  filter(signs_of_mental_illness == "True") %>% 
  group_by(Abbreviation) %>% 
  summarize(true_count = n())

prop_ill_false <- shoot_map %>% 
  filter(signs_of_mental_illness == "False") %>% 
  group_by(Abbreviation) %>% 
  summarize(false_count = n())

shoot_map <- left_join(shoot_map, prop_ill_false, by = "Abbreviation")
shoot_map <- left_join(shoot_map, prop_ill_true, by = "Abbreviation")

prop_ill_dat <- shoot_map %>% 
  select(false_count, true_count, long, lat, group, 
         Abbreviation, signs_of_mental_illness, fips.x, fips.y, state_count) %>% 
  group_by(Abbreviation) %>% 
  mutate(prop_true = (true_count / state_count) * 100,
         prop_false = (false_count / state_count) * 100)

## create counts of shootings by state 
shoot_count <- shoot_map %>% 
    group_by(Abbreviation) %>% 
    summarize(count = n())
armed_count <- shoot_map %>% 
    group_by(armed) %>% 
    summarize(count = n()) %>% arrange(desc(count))
armed_top_ten <- armed_count[-4,]
## reduce to top ten 
armed_top_ten <- armed_top_ten[1:10, 1]
armed_top_ten <- armed_top_ten$armed

## filter shoot data to only these top ten armed 
shoot_map <- shoot_map %>% filter(armed %in% armed_top_ten)

## plot map 
shoot_map_plot <- ggplot(shoot_map, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = count)) + coord_quickmap() + 
    labs(
        title = "Count of Fatal Police Shootings by US State"
    )

server <- function(input, output) {
##########Addintropage stuff
    
#################Mary-MaePage
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
        paste0("The race ",  high_race$race[1],  " and the gender ",  input$gender , " have the highest amount of fatal police shootings with ", high_race$shootings[1], " shootings. ") 

    })
  
    output$descripation_one <- renderText({
        print("This bar graph helps us look at the disparities of fatal police shootings among race. The user 
          can then adjust it to further look at the differences between different races and different genders. This allows us to 
              understand and look at the patterns of who is being fatally shot most by police.")
    })
##################End Mary-Mae Page
 ###################AmyPage
    output$map <- renderPlot({
       # subset <- prop_ill_dat %>% 
            #filter(armed %in% input$armed)
        shoot_map_plot <- ggplot(prop_ill_dat, aes(long, lat, group = group)) +
            geom_polygon(aes(fill = prop_true)) + coord_quickmap() + 
            labs(
                title = "Proportion of Individuals Fatally Shot and Mentally-Ill, by State"
            ) +  scale_fill_gradient2("Proportion of Total Shot")
        shoot_map_plot
    })
    
    output$map_descript <- renderText({
      print("This map plots the proportion of individuals fatally shot by police that were mentally ill. 
            States colored in with a lighter shade of purple have higher proportions than states colored 
            with a darker purple The plot suggests that states with the highest proportions of mentally ill 
            victims of police shootings were Wyoming, South Dakota, and Vermont. The states with the lowest
            proportions were Kentucky, Maine, and Montana. Overall, the map suggests that states in the 
            southern parts of the US have higher proportions of mentally ill individuals shot by policemen
            than northern states.")
    })
###################End Amy Page
    ##################AshleyPage
    output$mannerBar <- renderPlot({
        subset<- data %>%
            filter(gender %in% input$gender)
        ggplot(data= subset, aes(x= manner_of_death, fill= ))+ 
            geom_bar() + labs(title= "Manner of Death Bar Graph", x= "Manner of Death", y= "Occurrances")
    })
###################End Ashley Page
 #############conclusion
    output$conclusion <- renderText({
        # print("Throughout our project we looked at a lot of different aspects of this data. 
        #       To start, we look at the shooting counts among the different states. We found that ", shoot_count_high$Abbreviation[1], 
        #       " had the highest total shoot count with ", shoot_count_high$count[1], " fatal police shootings.
        #       We allow the user to switch between different things the person was armed (or not armed with) this showed a pattern that 
        #       a high majority of people who were shot were actually not armed. This number is alarming with ", armed_count$count[3], 
        #       " people being unarmed while they were fatally shot by the police. This shows that many police error on the side of caution
        #       and fear when shooting at citzens, which can be very problematic when it entails taking a human life.")
        print("Throughout our project, we aimed to examine aspects of the data that would provide insight to the characteristics 
        of the victims of police shootings. The first panel displays a USmap which is colored according to the proportion of individuals 
              who were fatally shot by policemen and were mentally ill (out of all individuals shot). This map gave evidence to suggest 
              states in the south had higher proportions of mentally ill individuals who were victimized, while states in the north had 
              a smaller proportion. This evidence suggests that there is a disproportionate approach as to how US policing systems issue order
              when confronting an individual with a mental illness. This suggests that nationwide training should be instilled in police forces
              to detail how to respond to calls involving mentally ill individuals. It is then that we can hope to reduce these numbers.") 
        br()
        print("In our next page, we showed a bar graph that showed the number of fatal shootings spiliting up by race. This showed us that in this data, the most people
              being shot were the ", by_race_high$race[1], " race and ", by_race_high$gender[1], " gender with ", by_race_high$shootings[1],
              " shootings. This number is closely followed by Black Males with ", by_race_high$shootings[2], " shootings. This shows us
              that Males are being shot more than females and in this particular data White Males more than Blakc Males. In our last page we
              created a bar graph that depicted that manner of death.")
        
        br()
        print("ADD ASHLEY DATA!!!! All of this data is very important because it allows us to investigate the Police force and think about the manner of fatal shootings. Are they always justified? With these 
              numbers we can see that although more people are armed than unarmed, there is still an alarming number of unarmed 
              individuals being shot. Furthermore we can start investigating deeper questions such as, who gets to decides if a person 
              lives or dies? Should the Police person have the ability to be the judge, jury and executor?")
        br()
        print("This data as a whole I believe was aliry unbiased. It was giving numbers and statistics which were not pulled out of thin air. The major issue that we had with this 
              data is that there were many catergories that did not have a variable in the box, like empty parts in the data. This made it 
              a little more diffcult when we constructed charts.")
        br()
        print("What's next with this project? Investigate another data set that shows 
              fatal Police shooting but during a different time period. We could compare and contrast the data and look how fatal Police shootings
              have changed over time." )
            
    })
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(type = "tabs", 
            ##Intro Page
            tabPanel("Introduction"),
            
            ##AMy Page
            tabPanel("Tab 2",
                     h2("Amy Page"),
                     mainPanel(
                         plotOutput("map"),
                         textOutput("map_descript")
                     )
    ),
    
            ###Mary-Mae Page
                tabPanel("Tab 3", 
                         h2("Mary-Mae Page"),
                         sidebarLayout(
                             sidebarPanel(
                                 uiOutput("gender"), 
                                 selectInput(inputId = "gender", "Select Gender:", 
                                             c("Female" = "F", 
                                               "Male" = "M")), 
                                 radioButtons(inputId = "Color", label = "Select Bar Color", 
                                              c("Red", "Blue", "Gray", "Black"), 
                                              selected = "Black"
                             )
                         ),
                            mainPanel(
                                plotOutput("race_bar"), 
                                textOutput("message"),
                                textOutput("descripation_one")
                                
                            )
                )
          ),
                ####Ashley Page
               ## tabPanel("Tab 4", 
                    ##     h2("Ashley Page"), 
                      ##   sidebarLayout(
                       ##      sidebarPanel(
                              ##   uiOutput("gender"), 
                               ##  selectInput(inputId = "gender", "Select Gender:", 
                                          ##   c("Female" = "F", 
                                             ##  "Male" = "M")), 
                                ## radioButtons(inputId = "Color", label = "Select Bar Color", 
                                            ##  c("Red", "Blue", "Gray", "Black"), 
                                             ## selected = "Black"
                         ##    )
                       ##  )
                    
             ##   ),
             ##   mainPanel(
                  ##  textOutput("mannerBar")
             ##   )
       ##  ),
                #####Conclusion
                    tabPanel("Conclusion",
                            h2("Conclusion"), 
                        mainPanel(
                            textOutput("Conclusion")
                        )    
                             )
)
)
shinyApp(ui = ui , server = server)

