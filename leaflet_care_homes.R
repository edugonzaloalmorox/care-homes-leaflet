###########################################
# Mapping the quality of adult social care
# 5 December 2018
# Data - Care Quality Commission
# @Edudin Gonzalo
###########################################




library(tidyverse)
library(readr)
library(leaflet)
library(janitor)
library(hrbrthemes)
library(knitr)
library(htmltools)
library(htmlwidgets)



# Load data

care_homes = read_csv("data/care-homes-2018-clean.csv")  %>% 
  filter(location_status == "active") %>%
  select(location_id, location_name, 
         location_latest_overall_rating, 
         location_longitude, 
         location_latitude, 
         location_region) 



# Table 

care_homes %>%
  filter(location_region != "Unspecified", !is.na(location_latest_overall_rating)) %>%
  rename(Region = location_region, Rating = location_latest_overall_rating) %>%
  group_by(Region) %>%
  tabyl(Region, Rating) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  kable()





# Plot 

pal <- colorFactor(palette = c("dodgerblue3", "aquamarine4", "darkorange1", "firebrick2"), 
                   levels = c("Outstanding", "Good", "Requires improvement", "Inadequate"))


# subset data frames
outstanding = care_homes %>% filter(location_latest_overall_rating == "Outstanding")
good = care_homes %>% filter(location_latest_overall_rating == "Good")
improvement = care_homes %>% filter(location_latest_overall_rating == "Requires improvement")
inadequate = care_homes %>% filter(location_latest_overall_rating == "Inadequate")



leaflet() %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addCircleMarkers(data = outstanding,
                   radius = 0.2,
                   weight = 0.75,
                   lng = ~location_longitude, lat = ~location_latitude, 
                   label = ~htmlEscape(location_name),
                   color = ~pal(location_latest_overall_rating),  group = "Outstanding") %>% 
  addCircleMarkers(data = good,
                   radius = 0.2,
                   weight = 0.75,
                   lng = ~location_longitude, lat = ~location_latitude, 
                   label = ~htmlEscape(location_name),
                   color = ~pal(location_latest_overall_rating), group = "Good")  %>% 
  addCircleMarkers(data = improvement, 
                   radius = 0.2,
                   weight = 0.75,
                   lng = ~location_longitude, lat = ~location_latitude, 
                   label = ~htmlEscape(location_name),
                   color = ~pal(location_latest_overall_rating), group = "Requires improvement")  %>% 
  addCircleMarkers(data = inadequate, 
                   radius = 0.2,
                   weight = 0.75,
                   lng = ~location_longitude, lat = ~location_latitude, 
                   label = ~htmlEscape(location_name),
                   color = ~pal(location_latest_overall_rating), group = "Inadequate") %>% 
  addLayersControl(overlayGroups = c("Outstanding", "Good", "Requires improvement", "Inadequate")) %>% 
  setView(lng = 1, lat = 53, zoom = 5.85)