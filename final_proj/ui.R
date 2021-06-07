library(shiny, warn.conflicts = FALSE)

ui <- fluidPage(
  tabsetPanel(type = "tabs", 
              ##Intro Page
              tabPanel("Introduction",
                       h2("Introduction"),
                       sidebarLayout(
                         sidebarPanel(
                           img(src = "img.jpg", height = 300, width = 250)
                         ),
                         mainPanel(
                           textOutput("intro")
                         ) 
                       )
              ),
              
              ##AMy Page
              tabPanel("Fatal Shootings among Mentally Ill",
                       h2("Map of Mentally-Ill Victims"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "x", "Select mentally-ill or not mentally-ill:",
                                       c("Mentally-ill" = "prop_true",
                                         "Not mentally-ill" = "prop_false"))
                         ),
                         mainPanel(
                           plotOutput("map"),
                           textOutput("map_descript")
                         )
                       )
                       
              ),
              
              ###Mary-Mae Page
              tabPanel("Fatal Shootings by Race", 
                       h2("Victims by Race and Gender"),
                       sidebarLayout(
                         sidebarPanel(
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
              tabPanel("Fatal Shootings by Manner of Death", 
                       h2("Victim Manner of Death by Race"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "race", "Select Race:", 
                                       c("Black" = "B", 
                                         "White" = "W",
                                         "Asian" = "A", 
                                         "Other" = "O", 
                                         "Hispanic" = "H", 
                                         "Native American" = "N"))
                           
                         ),
                         mainPanel(
                           plotOutput("mannerBar"),
                           textOutput("mannercomment"), 
                           textOutput("descripation")
                           
                         )
                       )
              ),
              
              #####Conclusion
              tabPanel("Conclusion",
                       h2("Conclusion"), 
                       mainPanel(
                         textOutput("conclusion1"),
                         textOutput("conclusion2"),
                         textOutput("conclusion3"),
                         textOutput("conclusion4"),
                         textOutput("conclusion5")
                       )    
              )
  )
)