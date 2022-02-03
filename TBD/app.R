### 1. Attach packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)

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
                                          "Continual" = "CONTINUAL")),
                 selectInput(inputId = "pt_color",
                             label = "Choose point color:",
                             choices = c("Awesome red!" = "red",
                                         "Beautiful blue!" = "blue",
                                         "Ragin' Orange!" = "orange"))
    ), # end sidebar panel
    
    mainPanel("put my graph in here",
              plotOutput(outputId = "penguin_plot"))
  ) # end sidebarLayout
)

### 3. create the server fxn
server <- function(input, output) {
  treatment_select <- reactive({
    fish_clean %>%
      select(year,count,treatment) %>% 
      group_by(year) %>% 
      filter(treatment == input$kelp_treatment)
  }) # end penguin_select reactive
  
  output$penguin_plot <- renderPlot({
    ggplot(data = treatment_select(), aes(x = year, y = count)) +
      geom_line(color = input$pt_color)
  })
}

### 4. combine into an app:
shinyApp(ui = ui, server = server)
