# Load required packages (install as needed)
library(tidyverse)
library(sf)
library(leaflet)
library(units)
library(RColorBrewer)

# ---- Create attribute table ----

# Load saved data
metricsFL <- read.csv("data/metrics_florida.csv")

# Perform calculations
metricsFL <- metricsFL %>%
  # calculate percentiles for each variable, ignoring NAs
  mutate(income_pct = if_else(is.na(income), NA_real_, percent_rank(income)*100),
         ssi_pct = if_else(is.na(ssi), NA_real_, percent_rank(ssi)*100),
         unemployed_pct = if_else(is.na(unemployed), NA_real_, percent_rank(unemployed)*100),
         costs_pct = if_else(is.na(costs), NA_real_, percent_rank(costs)*100),
         mobile_pct = if_else(is.na(mobile), NA_real_, percent_rank(mobile)*100),
         education_pct = if_else(is.na(education), NA_real_, percent_rank(education)*100),
         language_pct = if_else(is.na(language), NA_real_, percent_rank(language)*100),
         single_pct = if_else(is.na(single), NA_real_, percent_rank(single)*100),
         age_pct = if_else(is.na(age), NA_real_, percent_rank(age)*100),
         disabled_pct = if_else(is.na(disabled), NA_real_, percent_rank(disabled)*100),
         uninsured_pct = if_else(is.na(uninsured), NA_real_, percent_rank(uninsured)*100),
         lifeexpect_pct = if_else(is.na(lifeexpect), NA_real_, percent_rank(lifeexpect)*100)) %>%
  # flag if variable meets the criteria (>=80th or <=20th percentile, depending on variable)
  mutate(burden_income = if_else(income_pct >= 80, 1, 0, missing = 0),
         burden_ssi = if_else(ssi_pct >= 80, 1, 0, missing = 0),
         burden_unemployed = if_else(unemployed_pct >= 80, 1, 0, missing = 0),
         burden_costs = if_else(costs_pct >= 80, 1, 0, missing = 0),
         burden_mobile = if_else(mobile_pct >= 80, 1, 0, missing = 0),
         burden_education = if_else(education_pct >= 80, 1, 0, missing = 0),
         burden_language = if_else(language_pct >= 80, 1, 0, missing = 0),
         burden_single = if_else(single_pct >= 80, 1, 0, missing = 0),
         burden_age = if_else(age_pct >= 80, 1, 0, missing = 0),
         burden_disabled = if_else(disabled_pct >= 80, 1, 0, missing = 0),
         burden_uninsured = if_else(uninsured_pct >= 80, 1, 0, missing = 0),
         burden_lifeexpect = if_else(lifeexpect_pct <= 20, 1, 0, missing = 0)) %>%
  # calculate the total number of burdens experience in each census tract
  mutate(total_burdens = rowSums(across(burden_income:burden_lifeexpect), na.rm = TRUE)) %>%
  # create field to list each sociodemographic burden threshold met
  mutate(txt_income = ifelse(burden_income == 1, "Low income", NA),
         txt_ssi = ifelse(burden_ssi == 1, "Social security income", NA),
         txt_unemployed = ifelse(burden_unemployed == 1, "Unemployment", NA),
         txt_costs = ifelse(burden_costs == 1, "Housing cost stress", NA),
         txt_mobile = ifelse(burden_mobile == 1, "Mobile homes", NA),
         txt_education = ifelse(burden_education == 1, "Limited education", NA),
         txt_language = ifelse(burden_language == 1, "Linguistically isolated", NA),
         txt_single = ifelse(burden_single == 1, "Single-parent households", NA),
         txt_age = ifelse(burden_age == 1, "Vulnerable age groups", NA),
         txt_disabled = ifelse(burden_disabled == 1, "Disabilities", NA),
         txt_uninsured = ifelse(burden_uninsured == 1, "Lacking health insurance", NA),
         txt_lifeexpect = ifelse(burden_lifeexpect == 1, "Low life expectancy", NA)) %>%
  mutate(burdens = paste(txt_income,txt_ssi,txt_unemployed,txt_costs,txt_mobile,txt_education,
                         txt_language,txt_single,txt_age,txt_disabled,txt_uninsured,txt_lifeexpect, sep = ", ")) %>%
  mutate(burdens = gsub('NA, ', '', burdens)) %>%
  mutate(burdens = gsub(', NA', '', burdens)) %>%
  mutate(burdens = ifelse(burdens == "NA", "None", burdens)) %>%
  mutate(tract = as.character(tract)) %>%
  # remove the txt fields
  select(-(txt_income:txt_lifeexpect))


# ---- Merge with census tract map ----

# Load the saved tract map 
load(file = 'data/tracts_tampabay.RData')

# Reduce existing attribute table
tampabay <- tampabay %>%
  select(GEOID) %>%
  # rename tract ID field to align with metrics table
  rename(tract = GEOID)

# For each mapped census tract, pull the corresponding metrics
tampabay_metrics <- tampabay %>%
  left_join(metricsFL, by = "tract") %>%
  # remove tracts with a population of 0
  filter(population > 0)


# ---- Calculate tract sizes ----

tampabay_metrics <- tampabay_metrics %>%
  # project to NAD83 / Florida West (ftUS)
  st_transform(2236) %>%
  # calculate area in square miles
  mutate(area_mi2 = st_area(geometry) %>%
           set_units(mi^2) %>%
           as.numeric()) %>%
  # calculate population density
  mutate(density = population/area_mi2) %>%
  relocate(area_mi2, .after = population) %>%
  relocate(density, .after = area_mi2)


# ---- Create map of burdened communities ----

# Convert to WGS84 for viewing with leaflet
tampabay_metrics_wgs84 <- st_transform(tampabay_metrics, crs = 4326)

# Create color palette
pal_burdens <- colorNumeric(palette = brewer.pal(8, "YlOrRd"), domain = tampabay_metrics_wgs84$total_burdens)

# Plot the number of burdens identified, and show list of burdens upon click
map <- tampabay_metrics_wgs84 %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal_burdens(total_burdens),
    fillOpacity = 0.7,
    weight = 0.5,
    opacity = 1,
    color = "black",
    popup = ~paste("<b>Number of Burdens</b>:", total_burdens, "<br><b>Burdens</b>:", burdens)
  ) %>%
  addLegend(
    pal = pal_burdens,
    values = ~total_burdens,
    title = "No. of Burdens",
    position = "bottomright"
  )

# Save final community map as a RData object (R), shapefile (ArcGIS), and KML (Google Maps) 
community_map <- tampabay_metrics_wgs84
#save(community_map, file = 'data/community_map.RData')
#st_write(community_map, "data/community_map.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)
#st_write(community_map, "data/community_map.kml", driver = "KML", delete_dsn = TRUE)


# Save the leaflet map as an HTML file that can be shared for easy viewing outside of R
library(htmlwidgets)
#saveWidget(map, "community_map.html", selfcontained = TRUE)

