library(shiny, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(maps, warn.conflicts = FALSE)
library(rsconnect, warn.conflicts = FALSE)


data <- read.csv("fatal-police-shootings-data.csv.bz2")
# remove unnecessary variables
data$id <- 
  data$name <- 
  data$date <- 
  data$armed <- 
  data$age <- 
  data$city <- 
  data$threat_level <- 
  data$flee <- 
  data$body_camera <- 
  NULL

by_race <- data %>%
  group_by(race, gender) %>%
  summarise(shootings = n()) 

shoot_map_data <- left_join(shoot_map_data, fips_data, by = c("state" = "state_abbr"))

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
  select(state, signs_of_mental_illness)

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
prop_ill_map <- prop_ill_dat %>% 
  select(prop_true, prop_false, long, lat, group, fips.x, fips.y)

## create counts of shootings by state 
shoot_count <- shoot_map %>% 
  group_by(Abbreviation) %>% 
  summarize(count = n())


###Group by Manner of death
manner_death <- data %>%
  group_by(manner_of_death, race) %>%
  summarise(occurances = n())

server <- function(input, output) {
  ##########Addintropage stuff
  output$intro <- renderText({
    print("Our Data is collected by the Washington Post and accounts for every 
      fatal police shooting since Jan 1st 2015 to June 1st 2020. We found this located
      on the Kaggle website (https://www.kaggle.com/andrewmvd/police-deadly-force-usage-us/metadata) 
      and decided that this is what we wanted to focus on 
      energy on for the final project. This data can allow us to look at patterns 
      among fatal police shootings in the US and analysis thouse. This is important as, especially in the last couple
            of years, the amount of fatal police shootings have been surronding the media. This
            topic is one people should be paying attention to as we question: Should police 
            have the ability to be the jury, judge and executioner of US citzens? Should police being
            killing people they vow to protect? Looking at this data we can find patterns and see
            if the people being shot are putting the police in danger and demographic wise who is being shot
            most. Using this data we can start answering those bigger questions such as; are these shootings justified?")
  })
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
              understand and look at the patterns of who is being fatally shot most by police. *The changing color plot
              allows users with visual imparements to change to the color that works best for them.")
  })
  ##################End Mary-Mae Page
  ###################AmyPage
  tf_select <- reactive({
    prop_ill_map %>% 
      select(input$x, long, lat, group)
  })
  
  output$map <- renderPlot({
    shoot_map_plot <- ggplot(tf_select(), aes(long, lat, group = group)) +
      geom_polygon(aes_string(fill = input$x)) + coord_quickmap() + 
      labs(
        title = "Proportion of Individuals Fatally Shot by Mental State, by State"
      ) +  
      scale_fill_gradient2("Proportion of Total Shot") 
    
    shoot_map_plot
  })
  
  output$map_descript <- renderText({
    paste("This map plots the proportion of individuals fatally shot by police based on the state of their 
            mental health (mentally-ill or not mentally-ill).
            States colored in with a darker shade of purple have higher proportions than states colored 
            with a lighter shade. The widget allows the user to toggle between viewing the proportions of the total individuals 
            shot that were mentally-ill, and the proportions of the total shot that were not mentally-ill. 
            The plot suggests that states with the highest proportions of mentally ill 
            victims of police shootings were Wyoming, South Dakota, and Vermont. The states with the lowest
            proportions were Kentucky, Maine, and Montana. Overall, the map suggests that states in the 
            northern parts of the US have higher proportions of mentally ill individuals shot by policemen
            than southern states. A comparison of the maps allows us to discern that over all, the majority of individuals
                being fatally shot by policement are not mentally-ill. In some states, such as Rhode Island, 100%
                of the victims are not mentally-ill.")
    
  })
  ###################End Amy Page
  ##################AshleyPage
  
  race_name <- reactive({
    if(is.null(input$race)) {
      manner_death
    }  else {
      manner_death %>%
        filter(race %in% input$race) 
    }
  }) 
  output$mannerBar <- renderPlot({
    ggplot(race_name(), aes(manner_of_death, occurances ))+ 
      geom_col(col = "Red")+
      labs(title= "Manner of Death Bar Graph", x= "Manner of Death", y= "Occurrances")
  })
  
  output$mannercomment <- renderText({
    high_gender <- manner_death %>%
      filter(race == input$race) %>%
      arrange(desc(occurances))
    paste0("The race ",  input$race , " had ", high_gender$occurances[1]  ," ",  high_gender$manner_of_death[1] , " vs ", high_gender$occurances[2]  , " ",  high_gender$manner_of_death[2]) 
    
  })
  
  output$descripation <- renderText({
    print("This chart allows us to look at the way in which the individuals were killed by the police. The 
            options consit of being shot or being shot in tasered. This allows us to analyze the serverity of 
            the murder as see the patterns in which peoeple are being killed point blank vs given that taser warning.
            The user can then utlize the widget to change the race of the indivudial and compare how they numbers
            for each category change by race.")
  })
  ###################End Ashley Page
  #############conclusion
  output$conclusion1 <- renderText({
    paste0("Throughout our project, we aimed to examine aspects of the data that would provide insight to the characteristics 
        of the victims of police shootings. The first panel displays a USmap which is colored according to the proportion of individuals 
              who were fatally shot by policemen and were mentally ill (out of all individuals shot). This map gave evidence to suggest 
              states in the north had higher proportions of mentally ill individuals who were victimized, while states in the south had 
              a smaller proportion. This evidence suggests that there is a disproportionate approach as to how US policing systems issue order
              when confronting an individual with a mental illness.
              This suggests that nationwide training should be instilled in police forces
              to detail how to respond to calls involving mentally ill individuals. It is then that we can hope to reduce these numbers.")
    
  })
  output$conclusion2 <- renderText({
    paste0("In our next page, we showed a bar graph that showed the number of fatal shootings spiliting up by race. This showed us that in this data, the most people
              being shot were the ", by_race_high$race[1], " race and ", by_race_high$gender[1], " gender with ", by_race_high$shootings[1],
           " shootings. This number is closely followed by Black Males with ", by_race_high$shootings[2], " shootings. This shows us
              that Males are being shot more than females and in this particular data White Males more than Blakc Males. In our last page we
              created a bar graph that depicted that manner of death.")
  })
  output$conclusion3 <- renderText({
    paste0("All of this data is very important because it allows us to investigate the Police force and think about the manner of fatal shootings. Are they always justified? With these 
              numbers we can see that although more people are armed than unarmed, there is still an alarming number of unarmed 
              individuals being shot. Furthermore we can start investigating deeper questions such as, who gets to decides if a person 
              lives or dies? Should the Police person have the ability to be the judge, jury and executor?")
  })
  output$conclusion4 <- renderText({
    paste0("This data as a whole we believe was fairly unbiased. It was giving numbers and statistics which were not pulled out of thin air. The major issue that we had with this 
              data is that there were many catergories that did not have a variable in the box, like empty parts in the data. This made it 
              a little more diffcult when we constructed charts.
             ")
  })
  output$conclusion5 <- renderText({
    paste0("What's next with this project? Investigate another data set that shows 
              fatal Police shooting but during a different time period. We could compare and contrast the data and look how fatal Police shootings
              have changed over time.")
  })
}