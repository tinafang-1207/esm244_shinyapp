### 1. Attach packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)

fish <- read_csv(here("TBD", "data", "LTER_reef_fish.csv"))
fish_clean <- fish %>% 
  clean_names()


### 2. create user interface:
ui <- fluidPage(
  titlePanel("Fisheries"),
  sidebarLayout(
    sidebarPanel("put my widgets here",
                 radioButtons(inputId = "kelp_treatment",
                              label = "Choose treatment:",
                              choices = c("Control" = "CONTROL",
                                          "Annual" = "ANNUAL",
                                          "Continual" = "CONTINUAL"))
    ), # end sidebar panel
    
    mainPanel("put my graph in here",
              plotOutput(outputId = "treatment_plot"))
  ) # end sidebarLayout
)

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
