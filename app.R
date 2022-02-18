### 1. Attach packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(shinydashboard)
library(bslib)


fish <- read_csv(here("data", "LTER_reef_fish.csv"))
fish_clean <- fish %>% 
  clean_names()



### 2. create user interface:
ui <- fluidPage(theme = "theme.css",
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
                  tabPanel("Kelp", h3("Annual Kelp Counts in Each Treatment"),
                           radioButtons(inputId = "kelp_treatment",
                                        label = "Choose treatment:",
                                        choices = c("Control" = "CONTROL",
                                                    "Annual" = "ANNUAL",
                                                    "Continual" = "CONTINUAL")),
                           plotOutput(outputId = "treatment_plot"),
                           h3("Annual Kelp Counts in Each Year"),
                           selectInput(inputId = "kelp_treatment",
                                       label = "Choose year:",
                                       choices = c("2008":"2020")),
                           plotOutput(outputId = "treatment_plot"),
                           h3("Annual Kelp Counts in Each Site"),
                           selectInput(inputId = "kelp_treatment",
                                       label = "Choose Site:",
                                       choices = c("Arroyo Quemado Reef" = "AQUE",
                                                   "Carpinteria Reef" = "CARP",
                                                   "Mohawk Reef" = "MOHK",
                                                   "Naples Reef" = "NAPL",
                                                   "Isla Vista" = "IVEE")),
                           plotOutput(outputId = "treatment_plot")),
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
  treatment_select <- reactive({
    fish_clean %>%
      mutate(year = as.factor(year)) %>%
      mutate(count = as.factor(count)) %>%
      group_by(year,treatment) %>%
      summarize(count=n()) %>%
      filter(year %in% c("2010":"2020")) %>%
      filter(treatment == input$kelp_treatment)
  }) # end penguin_select reactive
  
  output$treatment_plot <- renderPlot({
    ggplot(data = treatment_select(), aes(x = year, y = count)) +
      geom_col()
  })
}

### 4. combine into an app:
shinyApp(ui = ui, server = server)
