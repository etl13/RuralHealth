
#Created by Elaona Lemoto
#Date May 27, 2025


##Need to load Swain_all data set and Swain_Business2 dataset
#The first dataset contains all the counts of datasets
#The second dataset contains the lat and long of actual business over time


library(shiny)
library(leaflet)
library(tidycensus)
library(sf)
library(dplyr)
library(stringr)
library(tidyr)

NC_business<-read.csv("data/Rural_Business_Marker.csv")
Rural_all<-readRDS("data/Rural_all.Rds")

# Define years and variables
available_years <- 2020:2023
acs_vars <- c(
  "Percent White" = "White", # Not Hispanic White
  "Percent Black African American" = "Black", # Not Hispanic Black African American
  "Percent American Indian and Alaska Native alone" = "American Indian", # Not Hispanic American Indian and Alaska Native alone
  "Percent Asian" = "Asian", # Not Hispanic Asian
  "Percent Native Hawaiian and Other Pacific Islander" = "NHPI", # Not Hispanic Native Hawaiian and Other Pacific Islander
  "Percent Some Other race alone" = "Other Race", # Not Hispanic Some Other race alone
  "Percent Two or more races" = "Two or More Races", # Not Hispanic Two or more races
  "Percent Hispanic" = "Hispanic", #  Hispanic
  "Percent Female" = "Percent Female", 
  "Count of Clinics" = "Count of Clinics",
  "Count of Hospitals" = "Count of Hospitals", 
  "Count of Dialysis Centers"= "Count of Dialysis Centers", 
  "Count of Pharmacies" = "Count of Pharmacies",
  "Count of Physicians and Surgeons" = "Count of Physicians and Surgeons",
  "Count of Urgent Care Centers" = "Count of Urgent Care Centers",
  "Count of Dentists"="Count of Dentists", 
  "Count of Grocery Stores" = "Count of Grocery Stores", 
  "Count of Convenience Stores"= "Count of Convenience Stores", 
  "Count of Exercise Facilites" = "Count of Exercise Facilities",
  "Count of Fast Food Restaurants"= "Count of Fast Food Restaurants", 
  "Count of Fire Departments"= "Count of Fire Departments", 
  "Count of Police Departments"= "Count of Police Departments", 
  "Count of Religous Institutions"= "Count of Religous Institutions", 
  "Count of Liquor Stores"="Count of Liquor Stores", 
  "Count of Tobacco Shops"= "Count of Tobacco Shops", 
  "Percent of HH Without Internet Access" = "No Internet Access", 
  "Percent of HH With Broadband Access" = "Broadband Access"
)




# UI
ui <- fluidPage(
  titlePanel("Mapping Health Access in North Carolina"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("selected_year", "Select ACS Year:",
                  min = min(available_years),
                  max = max(available_years),
                  value = min(available_years),
                  step = 1,
                  sep = "",
                  animate = animationOptions(interval = 1500, loop = TRUE)),
      selectInput("selected_variable", "Select ACS Variable:",
                  choices = names(acs_vars),
                  selected = "Median Household Income"),
      checkboxInput("show_businesses", "Show Businesses", value = TRUE)
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# Server
server <- function(input, output, session) {
  business_data_filtered <- reactive({
    req(input$show_businesses, input$selected_variable,input$selected_year)
    selected_var_code <- acs_vars[input$selected_variable]
    # Filter logic can go here if needed
    NC_business%>%
      filter(year==input$selected_year,variable_code == selected_var_code)
  })
  acs_data <- reactive({
    req(input$selected_year, input$selected_variable)
    selected_var_code <- acs_vars[input$selected_variable]
    
    data <- Rural_all %>%
      filter(year == input$selected_year, variable_code == selected_var_code) %>%
      mutate(estimate = as.numeric(estimate)) %>%
      filter(!is.na(estimate))
    
    # Ensure sf class is preserved
    if (!inherits(data, "sf")) {
      data <- st_as_sf(data)
    }
    
    return(data)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -80.793457, lat = 35.782169,, zoom = 7)
  })
  
  observe({
    data <- acs_data()
    pal <- colorNumeric("YlOrRd", domain = data$estimate, na.color = "#ccc")
    biz_data <- business_data_filtered()
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearMarkers()%>%
      addPolygons(
        fillColor = ~pal(estimate),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        popup = ~paste0(
          "<strong>GEOID: </strong>", census_tract, "<br>",
          "<strong>Estimate: </strong>", formatC(estimate, format = "d", big.mark = ",")
        )
      ) %>%
      addMarkers(data = biz_data,
                 lng = ~longitude,
                 lat = ~latitude,
                 popup = ~paste0("<strong>", company, "</strong>"))%>%
      clearControls() %>%
      addLegend("bottomright", pal = pal, values = data$estimate,
                title = input$selected_variable)
    
  })
}

# Run the app
shinyApp(ui, server)
