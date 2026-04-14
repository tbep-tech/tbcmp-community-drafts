# Load required packages (install as needed)
library(tidyverse)
library(sf)
library(leaflet)
library(units)
library(RColorBrewer)
library(viridis)


# ---- Create attribute table ----

# Load saved data
metricsFL <- read.csv("data/metrics_florida.csv")

# Perform calculations
metricsFL <- metricsFL %>%
  # calculate percentiles for each variable, ignoring NAs
  mutate(income_pct = if_else(is.na(income), NA_real_, percent_rank(income)*100),
         #ssi_pct = if_else(is.na(ssi), NA_real_, percent_rank(ssi)*100),
         #unemployed_pct = if_else(is.na(unemployed), NA_real_, percent_rank(unemployed)*100),
         costs_pct = if_else(is.na(costs), NA_real_, percent_rank(costs)*100),
         uninsured_pct = if_else(is.na(uninsured), NA_real_, percent_rank(uninsured)*100),
         mobile_pct = if_else(is.na(mobile), NA_real_, percent_rank(mobile)*100),
         plumbing_pct = if_else(is.na(plumbing), NA_real_, percent_rank(plumbing)*100),
         overcrowded_pct = if_else(is.na(overcrowded), NA_real_, percent_rank(overcrowded)*100),
         #education_pct = if_else(is.na(education), NA_real_, percent_rank(education)*100),
         #single_pct = if_else(is.na(single), NA_real_, percent_rank(single)*100),
         disabled_pct = if_else(is.na(disabled), NA_real_, percent_rank(disabled)*100),
         age_pct = if_else(is.na(age), NA_real_, percent_rank(age)*100),
         language_pct = if_else(is.na(language), NA_real_, percent_rank(language)*100),
         #lifeexpect_pct = if_else(is.na(lifeexpect), NA_real_, percent_rank(lifeexpect)*100),
         tri_pct = if_else(is.na(tri), NA_real_, percent_rank(tri)*100),
         superfunds_pct = if_else(is.na(superfunds), NA_real_, percent_rank(superfunds)*100),
         hazwaste_pct = if_else(is.na(hazwaste), NA_real_, percent_rank(hazwaste)*100)) %>%
  # flag if variable meets the criteria (>=80th or <=20th percentile, depending on variable)
  mutate(burden_income = if_else(income_pct >= 80, 1, 0, missing = 0),
         #burden_ssi = if_else(ssi_pct >= 80, 1, 0, missing = 0),
         #burden_unemployed = if_else(unemployed_pct >= 80, 1, 0, missing = 0),
         burden_costs = if_else(costs_pct >= 80, 1, 0, missing = 0),
         burden_uninsured = if_else(uninsured_pct >= 80, 1, 0, missing = 0),
         burden_mobile = if_else(mobile_pct >= 80, 1, 0, missing = 0),
         burden_plumbing = if_else(plumbing_pct >= 80, 1, 0, missing = 0),
         burden_overcrowded = if_else(overcrowded_pct >= 80, 1, 0, missing = 0),
         #burden_education = if_else(education_pct >= 80, 1, 0, missing = 0),
         burden_disabled = if_else(disabled_pct >= 80, 1, 0, missing = 0),
         burden_age = if_else(age_pct >= 80, 1, 0, missing = 0),
         burden_language = if_else(language_pct >= 80, 1, 0, missing = 0),
         #burden_single = if_else(single_pct >= 80, 1, 0, missing = 0),
         #burden_lifeexpect = if_else(lifeexpect_pct <= 20, 1, 0, missing = 0),
         burden_tri = if_else(tri_pct >= 80, 1, 0, missing = 0),
         burden_superfunds = if_else(superfunds_pct >= 80, 1, 0, missing = 0),
         burden_hazwaste = if_else(hazwaste_pct >= 80, 1, 0, missing = 0)) %>%
  # calculate the total number of burdens experienced in each census tract
  mutate(total_burdens = rowSums(across(burden_income:burden_hazwaste), na.rm = TRUE)) %>%
  # create field to list each burden threshold met
  mutate(txt_income = ifelse(burden_income == 1, "Low income", NA),
         #txt_ssi = ifelse(burden_ssi == 1, "Social security income", NA),
         #txt_unemployed = ifelse(burden_unemployed == 1, "Unemployment", NA),
         txt_costs = ifelse(burden_costs == 1, "Housing cost stress", NA),
         txt_uninsured = ifelse(burden_uninsured == 1, "Lacking health insurance", NA),
         txt_mobile = ifelse(burden_mobile == 1, "Mobile homes", NA),
         txt_plumbing = ifelse(burden_plumbing == 1, "Plumbing deficiencies", NA),
         txt_overcrowded = ifelse(burden_overcrowded == 1, "Over-crowed households", NA),
         #txt_education = ifelse(burden_education == 1, "Limited education", NA),
         txt_disabled = ifelse(burden_disabled == 1, "Disabilities", NA),
         txt_age = ifelse(burden_age == 1, "Vulnerable age groups", NA),
         txt_language = ifelse(burden_language == 1, "Linguistically isolated", NA),
         #txt_single = ifelse(burden_single == 1, "Single-parent households", NA),
         #txt_lifeexpect = ifelse(burden_lifeexpect == 1, "Low life expectancy", NA),
         txt_tri = ifelse(burden_tri == 1, "Proximity to toxic release sites", NA),
         txt_superfunds = ifelse(burden_superfunds == 1, "Proximity to Superfund sites", NA),
         txt_hazwaste = ifelse(burden_hazwaste == 1, "Proximity to hazardous waste sites", NA)) %>%
  mutate(burdens = paste(txt_income,txt_costs,txt_uninsured,txt_mobile,txt_plumbing,txt_overcrowded,txt_disabled,
                         txt_age,txt_language,txt_tri,txt_superfunds,txt_hazwaste, sep = ", ")) %>%
  mutate(burdens = gsub('NA, ', '', burdens)) %>%
  mutate(burdens = gsub(', NA', '', burdens)) %>%
  mutate(burdens = ifelse(burdens == "NA", "None", burdens)) %>%
  mutate(tract = as.character(tract)) %>%
  # remove the txt fields
  select(-(txt_income:txt_hazwaste))


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


# ---- (PROPOSED) Create map of burdened communities ----

tampabay_metrics <- tampabay_metrics %>%
  # calculate vulnerability index based on the weights applied to the 12 vulnerability criteria
  mutate(vulnerability = 
           (burden_income*1) + 
           (burden_mobile*1) + 
           (burden_disabled*1) + 
           (burden_tri*1) + 
           (burden_costs*0.5) + 
           (burden_uninsured*0.5) + 
           (burden_plumbing*0.5) + 
           (burden_overcrowded*0.5) + 
           (burden_age*0.5) + 
           (burden_language*0.5) + 
           (burden_superfunds*0.5) + 
           (burden_hazwaste*0.5)) %>%
  mutate(txt_income = ifelse(burden_income == 1, "Low income", NA),
         txt_costs = ifelse(burden_costs == 1, "Housing cost stress", NA),
         txt_uninsured = ifelse(burden_uninsured == 1, "Lacking health insurance", NA),
         txt_mobile = ifelse(burden_mobile == 1, "Mobile homes", NA),
         txt_plumbing = ifelse(burden_plumbing == 1, "Plumbing deficiencies", NA),
         txt_overcrowded = ifelse(burden_overcrowded == 1, "Over-crowded households", NA),
         txt_disabled = ifelse(burden_disabled == 1, "Disabled population", NA),
         txt_age = ifelse(burden_age == 1, "Vulnerable age groups", NA),
         txt_language = ifelse(burden_language == 1, "Linguistically isolated", NA),
         txt_tri = ifelse(burden_tri == 1, "Proximity to toxic release sites", NA),
         txt_superfunds = ifelse(burden_superfunds == 1, "Proximity to Superfund sites", NA),
         txt_hazwaste = ifelse(burden_hazwaste == 1, "Proximity to hazardous waste sites", NA)) %>%
  # list all burdens
  mutate(vul_burdens = paste(txt_income,txt_costs,txt_uninsured,txt_mobile,txt_plumbing,txt_overcrowded,txt_disabled,
                             txt_age,txt_language,txt_tri,txt_superfunds,txt_hazwaste, sep = ", ")) %>%
  mutate(vul_burdens = gsub('NA, ', '', vul_burdens)) %>%
  mutate(vul_burdens = gsub(', NA', '', vul_burdens)) %>%
  mutate(vul_burdens = ifelse(vul_burdens == "NA", "None", vul_burdens)) %>%
  # list burdens by dimension
  mutate(vul_economic = paste(txt_income,txt_costs,txt_uninsured, sep = ", ")) %>%
  mutate(vul_economic = gsub('NA, ', '', vul_economic)) %>%
  mutate(vul_economic = gsub(', NA', '', vul_economic)) %>%
  mutate(vul_economic = ifelse(vul_economic == "NA", "None", vul_economic)) %>%
  mutate(vul_demographic = paste(txt_mobile,txt_plumbing,txt_overcrowded, sep = ", ")) %>%
  mutate(vul_demographic = gsub('NA, ', '', vul_demographic)) %>%
  mutate(vul_demographic = gsub(', NA', '', vul_demographic)) %>%
  mutate(vul_demographic = ifelse(vul_demographic == "NA", "None", vul_demographic)) %>%
  mutate(vul_household = paste(txt_disabled,txt_age,txt_language, sep = ", ")) %>%
  mutate(vul_household = gsub('NA, ', '', vul_household)) %>%
  mutate(vul_household = gsub(', NA', '', vul_household)) %>%
  mutate(vul_household = ifelse(vul_household == "NA", "None", vul_household)) %>%
  mutate(vul_environment = paste(txt_tri,txt_superfunds,txt_hazwaste, sep = ", ")) %>%
  mutate(vul_environment = gsub('NA, ', '', vul_environment)) %>%
  mutate(vul_environment = gsub(', NA', '', vul_environment)) %>%
  mutate(vul_environment = ifelse(vul_environment == "NA", "None", vul_environment)) %>%
  mutate(tract = as.character(tract)) %>%
  select(-(txt_income:txt_hazwaste)) %>%
  relocate(vulnerability, .after = burdens) %>%
  relocate(vul_burdens, .after = vulnerability)
  
# Convert to WGS84 for viewing with leaflet
tampabay_metrics_wgs84 <- st_transform(tampabay_metrics, crs = 4326)

# Create color palette
pal_vulnerability <- colorNumeric(palette = brewer.pal(8, "YlGnBu"), domain = tampabay_metrics_wgs84$vulnerability)
pal_vulnerability <- colorNumeric(palette = c(brewer.pal(8, "YlGnBu"), "#000000"), domain = c(0, 8))
#pal_vulnerability <- colorNumeric(plasma(256), domain = tampabay_metrics_wgs84$vulnerability)

# Plot the number of burdens identified, and show list of burdens upon click
map <- tampabay_metrics_wgs84 %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal_vulnerability(vulnerability),
    fillOpacity = 0.7,
    weight = 0.5,
    opacity = 1,
    color = "white",
    popup = ~paste0(
      "<div style='
    font-family: Arial, sans-serif;
    font-size: 13px;
    line-height: 1.4;
    padding: 8px;
    min-width: 220px;
  '>",
      # Header
      "<div style='
    font-weight: bold;
    font-size: 20px;
    margin-bottom: 4px;
  '>Vulnerability Index</div>",
      "<div style='
    font-size: 20px;
    font-weight: bold;
    color: #2c7fb8;
    margin-bottom: 6px;'>", 
      vulnerability, "<span style='font-size:14px; font-weight:normal;'> / 8</span></div>",
      # Divider
      "<hr style='border: none; border-top: 1px solid #ddd; margin: 6px 0;'>",
      # Sections
      "<div style='margin-bottom:6px;'><b>Economic Burdens:</b> ", vul_economic, "</div>",
      "<div style='margin-bottom:6px;'><b>Household Burdens:</b> ", vul_household, "</div>",
      "<div style='margin-bottom:6px;'><b>Demographic Burdens:</b> ", vul_demographic, "</div>",
      "<div style='margin-bottom:6px;'><b>Environmental Burdens:</b> ", vul_environment, "</div>",
      "</div>")) %>%
    addLegend(
    pal = pal_vulnerability,
    values = c(0,8),
    title = "Flood Vulnerability",
    position = "bottomright"
  )
map

# Save final community map as a RData object (R), shapefile (ArcGIS), and KML (Google Maps) 
community_map <- tampabay_metrics_wgs84
#save(community_map, file = 'data/community_map.RData')
#st_write(community_map, "data/community_map.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)
#st_write(community_map, "data/community_map.kml", driver = "KML", delete_dsn = TRUE)


# Save the leaflet map as an HTML file that can be shared for easy viewing outside of R
library(htmlwidgets)
#saveWidget(map, "community_map.html", selfcontained = TRUE)


# ---- (ALL BURDENS) Create map of burdened communities ----

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

