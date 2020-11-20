#RAIS Tanzania,  Interactive Shiny Map// 


#set directories  -------------------------------------------------------------

#if(Sys.info()[["user"]] == "sandr") project_file_path <- "C:/Users/sandr/Dropbox/RAIS/RAIS/RAIS - Data comparison"
#if(Sys.info()[["user"]] == "sandr") data <- "C:/Users/sandr/Dropbox/RAIS (1)/RAIS/2018/RAIS 2015-2017 dashboard/tanzania-dashboard/data"

# RM: Shiny is the one time where I won't set explicit filepaths, as in server
#     may not exactly know the explicit filepath when making the app. So just
#     set to dashboard directory
#if(Sys.info()[["user"]] == "sandr")

#setwd("C:/Users/sandr/Dropbox/RAIS GIS/tanzania-dashboard - sandra/tanzania-dashboard -")

#install packages  -------------------------------------------------------------

#install.packages('rsconnect')
#install.packages('leaflet.extras')
#install.packages('shinydashboard')

#load packages    --------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(leaflet)
library(readxl)
library(dplyr)
library(RANN)
library(arulesViz)
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(geosphere)
library(leaflet.extras)
library(tidyr)
library(stringr) # RM: added this, for str_squish() 

#Link to shiny object ---------------------------------------------------------

rsconnect::setAccountInfo(name='buzinsky-impact-evaluations',
                          token='E14FFF283ADDC6298A4BA6FBFC0AC8FD',
                          secret='VZmjcDQ57SY9UGmSj1FC7XMmTUOLhCqh/XtVLBfJ')


#import data  ----------------------------------------------------------------

tz_app1 = read.csv(file.path("data", "RAIS data_OHS.csv")) # RM: Changed to "data" in file.path

health <- read.csv(file.path("data", "Facility_List_nodispen.csv"))

school <- read.csv(file.path("data", "schools_combined.csv"))


# Define User Interface for app that draws dashboard ---------------------------

header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(collapsed = FALSE,
                            sidebarMenu(
                              checkboxGroupInput("tab_year",    
                                                 label = "Year",
                                                 choices = c("2015" = 2015,
                                                             "2016" = 2016,
                                                             "2017" = 2017,
                                                             "2018" = 2018 ),
                                                 select=c("2015", "2016", "2017", "2018" )),
                              
                              
                              checkboxGroupInput("tab_lights",
                                                 label= "Accidents by Visibility",
                                                 choices = c("Day" = "Day",
                                                             "Night" = "Night",
                                                             "Smoke" = "Smoke",
                                                             "Street Lights" = "Street Lights",
                                                             "Twilight" = "Twilight"),
                                                 select = c("Day")),
                              
                              
                              checkboxGroupInput("tab_road",
                                                 label = "Accidents by Road Type",
                                                 choices = c("Bridge" = "Bridge" ,
                                                             "City Roads" = "City Roads",
                                                             "District Roads" = "District Roads" ,
                                                             "Regional Roads" = "Regional Roads",
                                                             "Rural Roads" = "Rural Roads",
                                                             "Trunk Roads" = "Trunk Roads"),
                                                 
                                                 select=c("City Roads","Bridge", "District Roads", "Regional Roads", "Rural Roads", "Trunk Roads")),
                              
                              checkboxGroupInput("tab_injury",
                                                 label   = "Accidents by Injury Type",
                                                 choices =  c("Light Injury" = "Light Injury",
                                                              "Severe Injury" = "Severe Injury",
                                                              "Fatality" = "Fatality") ),
                              
                              sliderInput("Severe.injuries", "Severe Injuries Involved in Accidents:",
                                          min = 0, max = 26,
                                          value = c(0, 26)),
                              sliderInput("Fatalities", "Fatalities Involved in Accidents:",
                                          min = 0, max = 34,
                                          value = c(0, 34)),
                              sliderInput("males", "Males Involved in Accidents:",
                                          min = 0, max = 60,
                                          value = c(0, 60)),
                              sliderInput("females", "Females Involved in Accidents:",
                                          min = 0, max = 30,
                                          value = c(0, 30)))
                                                                  )




body <- dashboardBody(
  tags$head(  tags$style(HTML(".main-sidebar { font-size: 9px; }")) %>%
                tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  fluidRow(
    box(width = 13, height = 800, title = "Tanzania Road Accident Information System (RAIS) 2015-2018",
        leafletOutput("accident_map", height = 700))
  )
)


ui <- dashboardPage(
  header,
  sidebar,
  body
)


# Define server logic ----------------------------------------------------------
server <- function(input, output, session){
  

# create leaflet ---------------------------------------------------------------

output$accident_map <- renderLeaflet({
  
  
  #load variables 
   tz_app1 <- separate(tz_app1, 
                            Date,
                            into = c("day_month", "year"),
                            sep = ",",
                            remove=FALSE
  )
  
  tz_app1$road <- tz_app1$Road.Class %>% as.character
  tz_app1$lighttime <- tz_app1$Light %>% as.character
  tz_app1$male <- tz_app1$Male %>% as.numeric
  tz_app1$female <- tz_app1$Female %>% as.numeric
  tz_app1$Fatalities <- tz_app1$Fatalities %>% as.numeric
  tz_app1$Severe.injuries <- tz_app1$Severe.injuries %>% as.numeric
  
  #make severity and fatalities dummy variables
  
  tz_app1$fatality_dummy <- ifelse(tz_app1$Fatalities == 0 , 0 , 1)
  tz_app1$severity_dummy <- ifelse(tz_app1$Severe.injuries == 0 , 0 , 1)
  tz_app1$light_dummy <- ifelse(tz_app1$Light.injuries == 0 , 0 , 1)
  
  #combine them together in a categorical var
  #tz_app1%>%
  #mutate(injury = ifelse(tz_app1$light_dummy == 1, "Light Injury", NA)) %>%
  #mutate(injury = ifelse(tz_app1$severity_dummy == 1, "Severe Injury", NA)) %>%
  #mutate(injury = ifelse(tz_app1$fatality_dummy == 1, "Fatality", NA))  
  
#tz_app1%>%
#    tz_app1$injury = ifelse(tz_app1$light_dummy == 1, "Light Injury", NA) %>% 
#     ifelse(tz_app1$severity_dummy == 1, "Severe Injury", NA) %>%
#     ifelse(tz_app1$fatality_dummy == 1, "Fatality", NA)  

tz_app1$injury[tz_app1$fatality_dummy == 1] <- "Fatality"
tz_app1$injury[tz_app1$severity_dummy ==1] <- "Severe Injury"
tz_app1$injury[tz_app1$light_dummy ==1] <- "Light Injury"

  
  
  #### Subsets
  tz_app1$year <- tz_app1$year %>% str_squish() %>% as.numeric()
  tz_app1 <- tz_app1[(tz_app1$year %in% as.numeric(input$tab_year)) & (tz_app1$road %in% input$tab_road) & (tz_app1$injury %in% input$tab_injury) & (tz_app1$lighttime %in% input$tab_lights)  & (tz_app1$male >= input$males[1]) & (tz_app1$male <= input$males[2]) & (tz_app1$female >= input$females[1]) &  (tz_app1$Severe.injuries >= input$Severe.injuries[1] & tz_app1$Severe.injuries <= input$Severe.injuries[2]) & (tz_app1$Fatalities >= input$Fatalities[1] & tz_app1$Fatalities <= input$Fatalities[2]) & (tz_app1$female <= input$females[2]),]

  tz_app1$Latitude <- tz_app1$Latitude %>% as.character %>% as.numeric
  tz_app1$Longitude <- tz_app1$Longitude %>% as.character %>% as.numeric
  
  tz_app1 <- tz_app1[tz_app1$Latitude != 0,]
  

  
  #drop any NAs among all three datasets
  tz_app1 <- tz_app1 %>%
    dplyr::filter(!is.na(Latitude),
                  !is.na(Longitude))
  
  health <- health %>%
    dplyr::filter(!is.na(Latitude),
                  !is.na(Longitude))
  school <- school %>%
    dplyr::filter(!is.na(lat),
                  !is.na(lon))
  
  
  
  #### Convert to Spatial Object
  coordinates(tz_app1) <- ~Longitude+Latitude
  crs(tz_app1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  coordinates(health) <-  ~Longitude+Latitude
  crs(health) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  

  coordinates(school) <- ~lon+lat
  crs(school) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  
  tz_app1$popup_text <- paste0("Number of Fatalities\n", tz_app1$Fatalities)
  
  
  tz_app1$popup_text <- paste(tz_app1$Latitude, tz_app1$Longitude, sep=",")
  
  
  
  
  leaflet() %>%
    addTiles() %>%
    addMarkers(data = tz_app1, clusterOptions = markerClusterOptions, popup = ~popup_text) %>%
    addCircles(data = health, popup = ~facility , color = "blue", group = "Health Facilities") %>%
    addCircles(data = school, popup = ~name_code, color = "green", group = "Schools") %>%
    addLayersControl(overlayGroups = c("Health Facilities", "Schools"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(c("Schools"))
  
  
  
})

}
  



# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)

