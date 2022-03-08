### 1. Attach packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(shinydashboard)
library(bslib)
library(lubridate)
library(naniar)
library(leaflet)

### Read in the data
fish <- read_csv(here("data", "LTER_reef_fish.csv")) %>% 
  clean_names()

inverts <- read_csv(here("data", "LTE_Quad_Swath.csv")) %>% 
  clean_names()

Sea_Urchin <- read_csv(here("data", "LTE_Urchin_All_Years_20210209.csv")) %>%
  clean_names()

sites <- read_csv(here("data", "LTER_sites.csv")) %>% 
  mutate(site_full = case_when(
    site == "AQUE" ~ "Arroyo Quemado Reef",
    site == "CARP" ~ "Carpinteria Reef",
    site == "MOHK" ~ " Mohawk Reef",
    site == "NAPL" ~ "Naples Reef",
    site == "IVEE" ~ "Isla Vista Reef"
  ))

usaLat <- 34.4208
usaLon <- -119.6982
usaZoom <- 8.5



### Clean data for Widget 2 (counts)
fish_inverts <- fish %>% 
  full_join(inverts)

fish_inverts_clean <- fish_inverts %>%
  select(year, treatment, count, group) %>%
  replace_with_na(replace = list(count = -99999)) %>%
  drop_na() %>%
  group_by(year,treatment,group) %>%
  summarize(total_number = sum(count))

### Clean data for Widget 3 (npp)
npp <- read_csv(here("data", "NPP_All_Year.csv")) %>% 
  clean_names() %>% 
  group_by(year, site, treatment) %>% 
  summarise(total_npp = sum(npp_season_g_c_m2_day))

### Clean data for Widget 4 (size distribution)
# Find most abundant species (top 5)

most_abundant_fish <- fish %>% 
  group_by(common_name) %>% 
  summarise(most_abundant_fish = sum(count)) %>%  
  slice_max(order_by = most_abundant_fish, n=5)

most_abundant_inverts <- inverts %>% 
  group_by(common_name) %>% 
  summarise(most_abundant_inverts = sum(count)) %>%  
  slice_max(order_by = most_abundant_inverts, n=5) 

# Filter through the original dataset
fish_size <- fish %>%
  filter(common_name %in% c("Senorita","Blacksmith","Painted Greenling","Kelp Bass","Black Surfperch")) %>%
  select(site, treatment, size, common_name) %>%
  replace_with_na_all(condition = ~.x == -99999) %>%
  group_by(common_name, site, treatment) %>%
  summarise(across(everything(), list(mean), na.rm = TRUE)) %>%
  drop_na()

inverts_size <- inverts %>%
  filter(common_name %in% c("Palm Kelp","Giant Key Hole Limpet","Oar Weed","Rock Scallop","Warty Sea Cucumber")) %>%
  select(site, treatment, size, common_name) %>%
  replace_with_na_all(condition = ~.x == -99999) %>%
  group_by(common_name, site, treatment) %>%
  summarise(across(everything(), list(mean), na.rm = TRUE)) %>%
  drop_na()

urchin_size <- Sea_Urchin %>%
  select(site, treatment, size, common_name) %>%
  replace_with_na_all(condition = ~.x == -99999) %>%
  group_by(common_name, site, treatment) %>%
  summarise(across(everything(), list(mean), na.rm = TRUE))

# Combine all the datasets above
fish_inverts_size <- fish_size %>%
  full_join(inverts_size)
fish_inverts_urchin_size <- fish_inverts_size %>%
  full_join(urchin_size)



### 2. create user interface:
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   bootswatch="sandstone"),
  navbarPage(img(src = "kelp.png", height = 140, width = 170),
             
             # Site info
              tabPanel("About",
                       sidebarLayout(
                              sidebarPanel(
                                          "Authors: Yutian Fang & Renee LaManna",
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
              tabPanel("Interactive Map",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "inputSite", label = "Select site:", multiple = TRUE, choices = sort(sites$site), selected = "AQUE"),
                           tags$h2(" ")),
                           mainPanel("This map displays the sites at which this data was selected. Select 1 or more sites to learn more about each one!",
                                     leafletOutput(outputId = "leafletMap")
                           ) # end main panel
                         ) # end sidebarLayout
                       ), # end tabPanel
                   
             tabPanel("Invertebrate, Fish, & Algae Counts",
                           sidebarLayout(
                             sidebarPanel( 
                               radioButtons(inputId = "group_select",
                                           label = "Select a category:",
                                           choices = c("Fish" = "FISH",
                                                       "Invertebrates" = "INVERT", 
                                                       "Algae" = "ALGAE")
                                           ) # end selectInput
                               ), # end sidebar panel
                            mainPanel( "Select a species group to see their total counts throughout all survey years. You can also see the
                                       difference between each of the kelp removal treatment. Notice the substantial decrease in counts across all groups
                                       in 2015? 2015 was a big El Nino year and Santa Barbara experienced some of their warmest winter sea surface temperatures",
                                      plotOutput(outputId = "species_plot")
                                      ) #end mainPanel
                            )#end sidebar Layout
                      ), #end tabPanel
                          

                   tabPanel("Net Primary Production",
                            sidebarLayout(
                            sidebarPanel(
                                checkboxGroupInput(
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
  
  # Widget 1 output
  data <- reactive({
    sites %>%
      filter(site %in% input$inputSite) 
  })
  
  output$leafletMap <- renderLeaflet({
    leaflet(data = data()) %>%
      setView(lat = usaLat, lng = usaLon, zoom = usaZoom) %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~site_info, label = ~site_full) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
    
    

  })
  
  # Widget 2 output
  category_select <- reactive ({
    fish_inverts_clean %>% 
      filter(group %in% input$group_select)
    }) #end category_select reactive
  
  output$species_plot <- renderPlot({
    ggplot(data = category_select (), aes(x = year, y = total_number)) +
      geom_line(aes(color = treatment, linetype = treatment)) + 
      scale_x_continuous(breaks=c(2008:2020)) +
      theme_minimal()
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
  
  # Widget 4 output
  species_select <- reactive ({
    fish_inverts_urchin_size %>% 
      filter(common_name %in% input$size_select)
  }) #end species_select reactive
  
  output$size_plot <- renderPlot({
    ggplot(data = species_select(), aes(x = site, y = size_1, fill = treatment)) +
      geom_bar(position = "dodge", stat = "identity") + 
      labs(x = "Site", y = "Mean Species Size") +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal()
  }) #end size_plot
    
}



### 4. combine into an app:
shinyApp(ui = ui, server = server)
