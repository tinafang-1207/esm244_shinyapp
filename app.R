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
  group_by(year,treatment,site) %>% 
  summarise(total_npp = sum(npp_season_g_c_m2_day))

### Clean data for Widget 4 (data table)
clean_table <- Sea_Urchin %>% 
  group_by(year, site, treatment, common_name) %>% 
  summarise(total_count_urchins = sum(count)) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(site = as.factor(site)) %>% 
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(common_name = as.factor(common_name)) %>% 
  mutate(total_count_urchins = as.integer(total_count_urchins))

  
  
  ### 2. create user interface:
ui <- fluidPage(
  theme = bs_theme(version =5,
                   bootswatch="sandstone"),
  navbarPage((img(src = "kelp.png", height = 140, width = 170)),
               
                 
              tabPanel("Home",
                       
                         mainPanel(includeMarkdown("www/home.md"),
                                   width = 10)),
              tabPanel("Site Map",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "inputSite", label = "Select site:", multiple = TRUE, choices = sort(sites$site), selected = "AQUE"),
                           tags$h2(" ")),
                           mainPanel("This map displays the sites at which this data was selected. Select 1 or more sites to learn more about each one!",
                                     leafletOutput(outputId = "leafletMap")
                           ) # end main panel
                         ) # end sidebarLayout
                       ), # end tabPanel
                   
             tabPanel("Species Abundance",
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
                                radioButtons(
                                  inputId = "site_select",
                                  label = "Choose Site:",
                                                   choices = c("Arroyo Quemado Reef" = "AQUE",
                                                    "Carpinteria Reef" = "CARP",
                                                    "Mohawk Reef" = "MOHK",
                                                    "Naples Reef" = "NAPL",
                                                    "Isla Vista" = "IVEE"))),
                              mainPanel(
                                plotOutput(outputId = "npp_plot")))),
  
                   tabPanel("Urchin Data",
                            fluidRow(
                              p("Sea urchins consume the holdfasts that keep kelp anchored to the seafloor which make scientists believe these two organisms
                                         strongly interact. If sea urchins are not kept in check by their associated predators, they could decimate a kelp forest. Use
                                         this interactive data table to explore the differences in urchin counts between sites, in different years, and between the two kelp 
                                         treatments", style="text-align:justify;color:white;background-color:steelblue;padding:15px;border-radius:10px"
                            )
                            ),
                              mainPanel( 
                                         width = 10,
                                         DT::dataTableOutput("mytable")
                              ) #end mainPanel
                            #end sidebar Layout
                    )#end tabPanel
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
      theme_minimal() +
      scale_color_manual(values = c("steelblue3", "seagreen", "mediumaquamarine")) 
  }) #end species_plot

   # Widget 3 output
   npp_select <- reactive ({
     npp %>%  
    filter(site %in% input$site_select)
  })
  
  output$npp_plot <- renderPlot(
    ggplot(data = npp_select(), aes(x = year, y = total_npp, fill = treatment)) +
      theme_minimal() +
      geom_area(alpha=0.6 , size=.5, colour="white") + 
      scale_fill_manual(values = c("steelblue3", "seagreen", "mediumaquamarine")) + 
      scale_x_continuous(breaks=c(2008:2020)) +
      labs(x = "Year", y = "Total Net Primary Production")
  ) 
  
  # Widget 4 output
  species_select <- reactive ({
    clean_table %>%  
      filter(common_name %in% input$name_select)
  })
  
  output$mytable = DT::renderDataTable(
    clean_table,
    filter = "top",
    colnames = c("Year", "Site", "Treatment", "Common name", "Count")
  )
    
}



### 4. combine into an app:
shinyApp(ui = ui, server = server)
