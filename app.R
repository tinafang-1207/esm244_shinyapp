### 1. Attach packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(shinydashboard)
library(bslib)
library(naniar)

### Read in the data
fish <- read_csv(here("data", "LTER_reef_fish.csv")) %>% 
  clean_names()

inverts <- read_csv(here("data", "LTE_Quad_Swath.csv")) %>% 
  clean_names()

fish_inverts <- fish %>% 
  full_join(inverts)

fish_inverts_clean <- fish_inverts %>%
  select(year, treatment, count, group) %>%
  replace_with_na(replace = list(count = -99999)) %>%
  drop_na() %>%
  group_by(year,treatment,group) %>%
  summarize(total_number = sum(count))

npp <- read_csv(here("data", "NPP_All_Year.csv")) %>% 
  clean_names() %>% 
  group_by(year, site, treatment) %>% 
  summarise(total_npp = sum(npp_season_g_c_m2_day))



### 2. create user interface:
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   bootswatch="sandstone"),
  navbarPage("LTER Kelp Removal Experiment in SBC",
             
             # Site info
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
                   
             tabPanel("Invertebrate, Fish, & Algae Counts",
                           sidebarLayout(
                             sidebarPanel( 
                               selectInput(inputId = "group_select",
                                           label = "Select a category:",
                                           choices = c("Fish" = "FISH",
                                                       "Invertebrates" = "INVERT", 
                                                       "Algae" = "ALGAE")
                                           ) # end selectInput
                               ), # end sidebar panel
                            mainPanel( "put my graph in here",
                                      plotOutput(outputId = "species_plot")
                                      ) #end mainPanel
                            )#end sidebar Layout
                      ), #end tabPanel
                          

                   tabPanel("Net Primary Production",
                            sidebarLayout(
                            sidebarPanel(
                                checkboxGroupInput("select",
                                                 inputId = "site_select",
                                                   label = "Choose Site:",
                                                   choices = c("Arroyo Quemado Reef" = "AQUE",
                                                    "Carpinteria Reef" = "CARP",
                                                    "Mohawk Reef" = "MOHK",
                                                    "Naples Reef" = "NAPL",
                                                    "Isla Vista" = "IVEE"))),
                              mainPanel(
                                plotOutput(outputId = "npp_plot")))),
  
                   tabPanel("Invertebrate, Fish, & Kelp Size Distribution",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "size_select",
                                            label = "Select a species:",
                                            choices = c("Black Surfperch" = "Black Surfperch",
                                                        "Blacksmith" = "Blacksmith",
                                                        "Kelp Bass" = "Kelp Bass",
                                                        "Painted Greenling" = "Painted Greenling",
                                                        "Senorita" = "Senorita",
                                                        "Giant Keyhole Limpet" = "Giant Key Hole Limpet",
                                                        "Oar Weed"= "Oar Weed",
                                                        "Palm Kelp" = "Palm Kelp",
                                                        "Rock Scallop" = "Rock Scallop",
                                                        "Warty Sea Cucumber" = "Warty Sea Cucumber",
                                                        "Purple Sea Urchin" = "Purple Urchin",
                                                        "Red Sea Urchin" = "Red Urchin"
                                                        )
                                ) # end selectInput
                              ), # end sidebar panel
                              mainPanel( "put my graph in here",
                                         plotOutput(outputId = "size_plot")
                              ) #end mainPanel
                            )#end sidebar Layout
                   ) #end tabPanel
              ) # end navbarpage
   ) # end UI


### 3. create the server fxn
server <- function(input, output) {
  #Widget 2 output
  species_select <- reactive ({
    # message("species_select, input$group_select = ", input$group_select)
    fish_inverts_clean %>% 
      filter(group %in% input$group_select)
    }) #end species_select reactive
  
  output$species_plot <- renderPlot({
    message("species_plot")
    ggplot(data = species_select(), aes(x = year, y = total_number)) +
      geom_line(aes(color = treatment, linetype = treatment)) + 
      scale_x_continuous(breaks=c(2008:2020))
  }) #end species_plot


 
   # Widget 3 output
   npp_select <- reactive ({
     npp %>%  
    filter(site %in% input$site_select)
  })
  
  output$npp_plot <- renderPlot(
    ggplot(data = npp_select(), aes(x = year, y = total_npp)) +
      theme_minimal() +
      geom_col(aes(color = treatment, fill=treatment)) + 
      scale_fill_manual(values = c("palegoldenrod", "lightskyblue2", "palegreen4")) + 
      scale_color_manual(values = c("palegoldenrod", "lightskyblue2", "palegreen4")) +
      scale_x_continuous(breaks=c(2008:2020)) +
      coord_flip()
  ) 
    
}



### 4. combine into an app:
shinyApp(ui = ui, server = server)
