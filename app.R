### 1. Attach packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(shinydashboard)
library(bslib)


fish <- read_csv(here("data", "LTER_reef_fish.csv")) %>% 
  clean_names()

inverts <- read_csv(here("data", "LTE_Quad_Swath.csv")) %>% 
  clean_names()

fish_inverts_kelp <- fish %>% 
  full_join(inverts)



### 2. create user interface:
ui <- fluidPage( theme = bs_theme(bootswatch="sandstone"),
                titlePanel("LTER Kelp Removal Experiment in SBC"),
                navlistPanel(              
                  tabPanel("About",
                           sidebarLayout(
                             sidebarPanel("Authors: Yutian Fang & Renee LaManna",
                                          br(),
                                          " ",
                                          br(),
                                          "Fang is a current PhD student, while LaManna is a current Masters student at the Bren School of Environmental Science
                                         & Management."
                             ),
                             mainPanel("This app visualizes data on fish, invertebrates, and algae over the course of 12 years given different treatments of kelp removal at 5 different
                                Santa Barbara Channel LTER sites.",
                                br(),
                                " ",
                                br(),
                                "The Santa Barbara Long-Term Ecological Research site was established in 2000 to better understand 
                                the ecology of kelp forests in this regions. SBC-LTER is based at the University of California, Santa Barbara.")
                           )),
                  tabPanel("Interactive Map", h3("Interactive Map for users to select kelp, invertebrate and fish abundance")),
                  
                  tabPanel("Invertebrate, Fish, & Algae Counts", h3("Species Counts in Each Treatment"),
                           radioButtons(inputId = "treatment",
                                        label = "Select a kelp removal treatment:",
                                        choices = c("Control" = "CONTROL",
                                                    "Annual" = "ANNUAL",
                                                    "Continual" = "CONTINUAL")),
                           selectInput(inputId = "group",
                                       label = "Select a category:",
                                       choices = c("Fish","Invertebrates", "Kelp")),
                           
                           
                           mainPanel("put my graph here", # Adding things to the main panel
                                     plotOutput(outputId = "species_plot"))),
                          
                            
                    tabPanel("Invertebrate", h3("Annual Invertebrate Counts in Each Treatment"),
                           radioButtons(inputId = "kelp_treatment",
                                        label = "Choose treatment:",
                                        choices = c("Control" = "CONTROL",
                                                    "Annual" = "ANNUAL",
                                                    "Continual" = "CONTINUAL")),
                           plotOutput(outputId = "treatment_plot"),
                           h3("Annual Invertebrate Counts in Each Year"),
                           selectInput(inputId = "kelp_treatment",
                                       label = "Choose year:",
                                       choices = c("2008":"2020")),
                           plotOutput(outputId = "treatment_plot"),
                           h3("Annual Invertebrate Counts in Each Site"),
                           selectInput(inputId = "kelp_treatment",
                                       label = "Choose Site:",
                                       choices = c("Arroyo Quemado Reef" = "AQUE",
                                                   "Carpinteria Reef" = "CARP",
                                                   "Mohawk Reef" = "MOHK",
                                                   "Naples Reef" = "NAPL",
                                                   "Isla Vista" = "IVEE")),
                           plotOutput(outputId = "treatment_plot")),
                
                    tabPanel("Reef Fish", 
                           h3("Annual Fish Counts in Each Treatment"),
                           radioButtons(inputId = "kelp_treatment",
                                        label = "Choose treatment:",
                                        choices = c("Control" = "CONTROL",
                                                    "Annual" = "ANNUAL",
                                                    "Continual" = "CONTINUAL")),
                           plotOutput(outputId = "treatment_plot"),
                           h3("Annual Fish Counts in Each Year"),
                           selectInput(inputId = "kelp_treatment",
                                       label = "Choose year:",
                                       choices = c("2008":"2020")),
                           plotOutput(outputId = "treatment_plot"),
                           h3("Annual Fish Counts in Each Site"),
                           selectInput(inputId = "kelp_treatment",
                                       label = "Choose Site:",
                                       choices = c("Arroyo Quemado Reef" = "AQUE",
                                                   "Carpinteria Reef" = "CARP",
                                                   "Mohawk Reef" = "MOHK",
                                                   "Naples Reef" = "NAPL",
                                                   "Isla Vista" = "IVEE")),
                           plotOutput(outputId = "treatment_plot")),
                ))


### 3. create the server fxn
server <- function(input, output) {
  species_select <- reactive ({
    fish_inverts_kelp %>% 
      group_by(year, treatment) %>% 
      summarise(count = n()) %>% 
      filter(treatment == input$treatment) %>% 
      filter(group == input$group)
      
    
    })
  
  output$species_plot <- renderPlot({
    
    ggplot(data = species_select(), aes(x = year, y = count)) +
      geom_line() + 
      scale_x_continuous(breaks=c(2008:2020)) 
  
    
  })
  
}
shinyApp(ui = ui, server = server)


### 4. combine into an app:
shinyApp(ui = ui, server = server)
