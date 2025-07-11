
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


NC_business<-read.csv("data/NC_business_71125.csv")
# # Define years and variables
# available_years <- 2020:2023
# acs_vars <- c(
#   "Total Population" = "B01003_001",
#   "Median Age" = "B01002_001",
#   "All"  = "B03002_001E", # Total Population
#   "Not Hispanic" = "B03002_002E", # Not Hispanic
#   "Not Hispanic White" = "B03002_003E", # Not Hispanic White
#   "Not Hispanic Black African American" = "B03002_004E", # Not Hispanic Black African American
#   "Not Hispanic American Indian and Alaska Native alone" = "B03002_005E", # Not Hispanic American Indian and Alaska Native alone
#   "Not Hispanic Asian" = "B03002_006E", # Not Hispanic Asian
#   "Not Hispanic Native Hawaiian and Other Pacific Islander" = "B03002_007E", # Not Hispanic Native Hawaiian and Other Pacific Islander
#   "Not Hispanic Some Other race alone" = "B03002_008E", # Not Hispanic Some Other race alone
#   "Not Hispanic Two or more races" = "B03002_009E", # Not Hispanic Two or more races
#   "Hispanic" = "B03002_012E" #  Hispanic
# )
# 
# Swain_DB<-c()
# for (yr in 2020:2021) {
#   ACSdata <- get_acs(
#     geography = "block group",
#     variables = acs_vars,
#     state = "NC",
#     survey = "acs5",
#     county = c("Swain", "Vance",  "Durham"),
#     output = "wide",
#     year = 2020,
#     geometry = T
#   )%>%
#     select(-ends_with("M"))%>%
#     mutate(year = paste(yr))%>%
#     separate(NAME, c("census_block", "census_tract",
#                      "county", "state"), sep = c(","), rem = F)
#   
#   
#   Swain_DB <- rbind(Swain_DB, ACSdata)
# }
# 
# for (yr in 2022:2023) {
#   ACSdata <- get_acs(
#     geography = "block group",
#     variables = acs_vars,
#     state = "NC",
#     survey = "acs5",
#     county = c("Swain", "Vance",  "Durham"),
#     year = yr,
#     output = "wide",
#     geometry = T
#   )%>%
#     select(-ends_with("M"))%>%
#     mutate(year = paste(yr))%>%
#     separate(NAME, c("census_block", "census_tract", "county", "state"), sep = c(";"), rem = F)
#   
#   Swain_DB <- rbind(Swain_DB, ACSdata)
# }
# 
# Swain_DBC<-Swain_DB%>%
#   mutate(`White` = (`Not Hispanic White`/ All )*100 ,
#          `Black` = (`Not Hispanic Black African American`/ All )*100 ,
#          `American Indian` = (`Not Hispanic American Indian and Alaska Native alone`/ All)*100,
#          `Asian` = (`Not Hispanic Asian` /All)*100,
#          `NHPI` = (`Not Hispanic Native Hawaiian and Other Pacific Islander`/All )*100,
#          `Other Race` = (`Not Hispanic Some Other race alone` /All)*100,
#          `Two or More Races` = (`Not Hispanic Two or more races`/All)*100,
#          `Hispanic` = (`Hispanic`/All)*100)%>%
#   select(-c( geometry, county, state, `Total PopulationE`, GEOID, NAME, `Median AgeE`, All), -starts_with("Not"))%>%
#   pivot_longer(cols = !c(geometry,census_tract, census_block, year), names_to = "variable_code", values_to = "estimate")
# 
# #  
#  saveRDS(Swain_DBC, "~/Library/CloudStorage/Box-Box/SEED_County/New_Data/ACS_County/County_Demo.csv")

Swain_DBC<-readRDS(file = "data/County_Demo_71125.csv")


business_20_24 <- NC_business%>%
  dplyr::rename( 
    year = archive_version_year
  )%>%
  mutate(county_code=ifelse(county_code==173, "Swain County", 
                            ifelse(county_code==87, "Haywood County", 
                                   ifelse(county_code==69,"Franklin County", "Vance County"))))

#rename columns
business_20_24c <- business_20_24 %>%
  select(primary_sic_code, company, sic6_descriptions, year, city,
         census_tract, census_block, county_code
  )%>%
  filter(!is.na(sic6_descriptions), !(year==2024))

#rename columns
business_20_24c <- business_20_24 %>%
  select(primary_sic_code, company, sic6_descriptions, year, city,
         census_tract, census_block, county_code
  )%>%
  filter(!is.na(sic6_descriptions), !(year==2024))
#rename columns
business_20_24c <- business_20_24 %>%
  select(primary_sic_code, company, sic6_descriptions, year, city,
         census_tract, census_block, county_code
  )%>%
  filter(!is.na(sic6_descriptions), !(year==2024))

pharmacy<-591205
pharm<-business_20_24c%>%
  filter(primary_sic_code==pharmacy)%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Pharmacies")

pharm2<-business_20_24%>%
  filter(primary_sic_code==pharmacy)%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Pharmacies")

dentists<-802101
dent<-business_20_24c%>%
  filter(primary_sic_code==dentists)%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Dentists")

dent2<-business_20_24%>%
  filter(primary_sic_code==dentists)%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Dentists")

phys<-801101
phys_surg<-business_20_24c%>%
  filter(primary_sic_code==phys)%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Physicians and Surgeons")

phys_surg2<-business_20_24%>%
  filter(primary_sic_code==phys)%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Physicians and Surgeons")


clinics<-801104
clin<-business_20_24c%>%
  filter(primary_sic_code==clinics)%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Clinics")


clin2<-business_20_24%>%
  filter(primary_sic_code==clinics)%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Clinics")


dial<-809203
dialysis<-business_20_24c%>%
  filter(primary_sic_code==dial)%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Dialysis Centers")


dialysis2<-business_20_24%>%
  filter(primary_sic_code==dial)%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Dialysis Centers")

amb<-411902
ambl<-business_20_24c%>%
  filter(primary_sic_code==amb)%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Ambulance Services")


ambl2<-business_20_24%>%
  filter(primary_sic_code==amb)%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Ambulance Services")

hosp<-806202
hospital<-business_20_24c%>%
  filter(primary_sic_code==hosp)%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Hospitals")

hospital2<-business_20_24%>%
  filter(primary_sic_code==hosp)%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Hospitals")

Health<-rbind(hospital, ambl, dialysis,
              clin, phys_surg, dent, pharm)

## Need to remove Target RWE
# want all groceries
grocers <- business_20_24c %>%
  filter(primary_sic_code %in% c("541101", "541105"),
         str_detect(company, c("\\bTARGET\\b|\\bWALMART SUPERCENTER\\b"))
  )%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Grocery Stores")

# want all groceries
grocers2 <- business_20_24 %>%
  filter(primary_sic_code %in% c("541101", "541105") |
           str_detect(company, c("\\bTARGET\\b|\\bWALMART SUPERCENTER\\b"))
  )%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Grocery Stores")



# Religious Institutions 


relig <- business_20_24c %>%
  filter(primary_sic_code %in% c("866103", "866104", "866107", "866110", 
                                 "866112", "866113", "866114", "866116", 
                                 "866117", "866122", "866127", "866129",
                                 #Added clergy
                                 "866106"))%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Religious Institutions")


relig2 <- business_20_24 %>%
  filter(primary_sic_code %in% c("866103", "866104", "866107", "866110", 
                                 "866112", "866113", "866114", "866116", 
                                 "866117", "866122", "866127", "866129",
                                 #Added clergy
                                 "866106"))%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Religious Institutions")


# Liquor Stores

liquor <- business_20_24c %>%
  filter(primary_sic_code %in% c("592102", "592103", "592104"))%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Liquor Stores")


liquor2 <- business_20_24 %>%
  filter(primary_sic_code %in% c("592102", "592103", "592104"))%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Liquor Stores")
# TOBACCO


tobacco <- business_20_24c %>%
  filter(primary_sic_code %in% c("599301", "599306"))%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Tobacco Shops")


tobacco2 <- business_20_24 %>%
  filter(primary_sic_code %in% c("599301", "599306"))%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Tobacco Shops")

# Safety

fire <- business_20_24c %>%
  filter(primary_sic_code %in% c("922404", "922403", "922102")
  )%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Fire Departments")


fire2 <- business_20_24 %>%
  filter(primary_sic_code %in% c("922404", "922403", "922102")
  )%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Fire Departments")


police <- business_20_24c %>%
  filter(primary_sic_code %in% c("922103", "922104"))%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Police Departments")

police2 <- business_20_24 %>%
  filter(primary_sic_code %in% c("922103", "922104"))%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Police Departments")

## convenience stores

# Should we include gas station servicice stations?
convenience.stores <- business_20_24c %>%
  filter(primary_sic_code %in% c("541103")
  )%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Convenience Stores")



convenience.stores2 <- business_20_24 %>%
  filter(primary_sic_code %in% c("541103")
  )%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Convenience Stores")

#Fast Food


# want all fast food in Swain County 
fast.food <- business_20_24c %>%
  filter(company %in% c("ARBY'S", "BISCUITVILLE", "BOJANGLES' FAMOUS CHICKEN", "BURGER KING",
                        "CAPTAIN D'S SEAFOOD", "CHICKEN HUT", "CHICK-FIL-A", "CHUBBY'S TACOS",
                        "CHURCH'S CHICKEN", "COOK OUT", "DOG HOUSE", "FIVE GUYS",
                        "HARDEE'S", "HARDEE'S OF RESEARCH", "JIMMIE'S FAMOUS HOTDOGS",
                        "JIMMY'S FAMOUS HOTDOGS", "KFC", "MC DONALD'S", "POPEYES LOUISIANA KITCHEN", 
                        "Q SHACK", "TACO BELL", "WENDY'S", "ZAXBY'S CHICKEN FINGERS",
                        "WING STREET")
  )%>%
  group_by(year, census_tract, census_block)%>%
  count()%>%
  ungroup()%>%
  rename('estimate' = 'n')%>%
  mutate(variable_code = "Count of Fast Food Restaurants")


fast.food2<-business_20_24%>%
  ungroup()%>%
  filter(company %in% c("ARBY'S", "BISCUITVILLE", "BOJANGLES' FAMOUS CHICKEN", "BURGER KING",
                        "CAPTAIN D'S SEAFOOD", "CHICKEN HUT", "CHICK-FIL-A", "CHUBBY'S TACOS",
                        "CHURCH'S CHICKEN", "COOK OUT", "DOG HOUSE", "FIVE GUYS",
                        "HARDEE'S", "HARDEE'S OF RESEARCH", "JIMMIE'S FAMOUS HOTDOGS",
                        "JIMMY'S FAMOUS HOTDOGS", "KFC", "MC DONALD'S", "POPEYES LOUISIANA KITCHEN", 
                        "Q SHACK", "TACO BELL", "WENDY'S", "ZAXBY'S CHICKEN FINGERS",
                        "WING STREET"))%>%
  mutate(latitude =`latitude`, 
         longitude = `longitude`,
  )%>%
  select(company, year, latitude, longitude)%>%
  mutate(variable_code = "Count of Fast Food Restaurants")


Swain_Business<-rbind(hospital, ambl, dialysis,
                      clin, phys_surg, dent, pharm, grocers, 
                      relig, liquor, tobacco, fire, police, convenience.stores, 
                      fast.food)

Swain_Business2<-rbind(hospital2, ambl2, dialysis2,
                       clin2, phys_surg2, dent2, pharm2, grocers2, 
                       relig2, liquor2, tobacco2, fire2, police2, convenience.stores2, 
                       fast.food2)

Swain_Geo<-Swain_DBC%>%
  select(year, census_tract, census_block,  geometry)%>%
  mutate(year = as.numeric(year))%>%
  group_by_all()%>%
  count()%>%
  select(-n)%>%
  ungroup()


Swain_Business1<-Swain_Business%>%
  mutate(census_tract = as.numeric(census_tract))%>%
  mutate(census_tract = census_tract/100,
         census_block = paste("Block Group", census_block, sep = " "), 
         census_tract = paste(" Census Tract", census_tract, sep = " ") )%>%
  left_join(Swain_Geo, by = c("year", "census_tract","census_block"))


Swain_Business2<-Swain_Business2%>%
  mutate(year = as.numeric(year))%>%
  filter(!(year==2024))

Swain_Business<-st_sf(Swain_Business1)
Swain_DBC<-st_sf(Swain_DBC)

Swain_all<-rbind(Swain_DBC, Swain_Business)

Swain_all<-Swain_all%>%
  mutate(year= as.numeric(year))




# Define years and variables
available_years <- 2020:2023
acs_vars <- c(
  "Not Hispanic White" = "White", # Not Hispanic White
  "Not Hispanic Black African American" = "Black", # Not Hispanic Black African American
  "Not Hispanic American Indian and Alaska Native alone" = "American Indian", # Not Hispanic American Indian and Alaska Native alone
  "Not Hispanic Asian" = "Asian", # Not Hispanic Asian
  "Not Hispanic Native Hawaiian and Other Pacific Islander" = "NHPI", # Not Hispanic Native Hawaiian and Other Pacific Islander
  "Not Hispanic Some Other race alone" = "Other Race", # Not Hispanic Some Other race alone
  "Not Hispanic Two or more races" = "Two or More Races", # Not Hispanic Two or more races
  "Hispanic" = "Hispanic", #  Hispanic
  "Count of Ambulance Services" = "Count of Ambulance Services", 
  "Count of Clinics" = "Count of Clinics",
  "Count of Hospitals" = "Count of Hospitals", 
  "Count of Dialysis Centers"= "Count of Dialysis Centers", 
  "Count of Convenience Stores"= "Count of Convenience Stores", 
  "Count of Dentists"="Count of Dentists", 
  "Count of Fast Food Restaurants"= "Count of Fast Food Restaurants", 
  "Count of Fire Departments"= "Count of Fire Departments", 
  "Count of Grocery Stores" = "Count of Grocery Stores", 
  "Count of Liquor Stores"="Count of Liquor Stores", 
  "Count of Pharmacies"="Count of Pharmacies", 
  "Count of Physicians and Surgeons"="Count of Physicians and Surgeons",
  "Count of Police Departments"= "Count of Police Departments", 
  "Count of Religious Institutions"= "Count of Religious Institutions", 
  "Count of Tobacco Shops"= "Count of Tobacco Shops"
)




# UI
ui <- fluidPage(
  titlePanel("Mapping Health Access in NC: Swain, Durham, and Vance County"),
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
    Swain_Business2%>%
      filter(year==input$selected_year,variable_code == selected_var_code)
  })
  acs_data <- reactive({
    req(input$selected_year, input$selected_variable)
    selected_var_code <- acs_vars[input$selected_variable]
    
    data <- Swain_all %>%
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
